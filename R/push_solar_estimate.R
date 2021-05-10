library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(lubridate)

create_message <- function(solar_data_df){
  days_in_current_month <- Sys.Date() %>% strftime(format="%d") %>% as.numeric()
  current_month_string <-  Sys.Date() %>% strftime(format="%B")
  current_month <- 
    Sys.Date() %>% 
    strftime(format = "%Y-%m") %>% 
    lubridate::ym()
  
  solar_value_this_month <-  
    solar_data_df %>% 
    filter(year_month == current_month) %>% 
    pull(estimated_energy_utilised_value) %>% 
    round(2)
  
  estimated_feed_in_tarrif_this_month <-
    solar_data_df %>% 
    filter(year_month == current_month) %>% 
    pull(estimated_feed_in_tarrif_value) %>% 
    round(2)
    
  
  confidence_inteval <- 
    (
      filter(
        solar_data_df, 
        year_month == current_month
      )$estimated_energy_utilised_value 
      - 
      filter(
        solar_data_df, 
        year_month == current_month
        )$lower_confint_value
    )
  
  list(
    title_text = paste0(
      "Solar value estimate for ",  
      filter(solar_data_df, !is_training)$year_month, 
      " so far: €",  solar_value_this_month,
      " (± €", round(confidence_inteval, 2), ")"
    ), 
    body_text = paste0(
      current_month_string, " Mean Value per Day: €",
      round(solar_value_this_month / days_in_current_month, 2),
      "\n",
      "Estimated Feed In Tarrif: €", estimated_feed_in_tarrif_this_month,
      "\n",
      "Since ",
      filter(solar_data_df, is_training & year_month == min(year_month))$year_month, 
      " estimated total value from solar panels: €",
      round(sum(solar_data_df$estimated_energy_utilised_value, na.rm = TRUE)),
      "\n",
      "Total Carbon Offset: ", 
      round(sum(solar_data_df$fitted_values) * PROVIDER_CARBON_INTENSITY, 0), " kg CO2 equivalent"
    )
  )
}

# Constants

ELECTRICITY_PRICE = 0.20
FEED_IN_TARRIF_PRICE = 0.7
ESTIMATED_SELF_USE_RATE = 0.75
PROVIDER_CARBON_INTENSITY = 0.288  # kg CO2 / kWh (SSE for 2020)


# Import
report <- jsonlite::fromJSON("https://prodapi.metweb.ie/monthly-data/Dunsany%20(Grange)")

monthly_solar_generation <- dplyr::tibble(
  year = c(rep("2021", 12), rep("2020", 12), rep("2019", 12), rep("2018", 12)),
  month = rep(1:12, 4),
  solar_power_generation = c(
    c(85, 157.5, 280.7, 471.6, rep(NA, 8)),
    c(rep(NA, 5), 358.4, 394.9, 314.3, 332.8, 235, 119.6, 38.7),
    rep(NA, 12),
    rep(NA, 12)
  )
)


# Clean
dunsany_solar_radiation <- 
  report$solar_radiation$report %>% 
  purrr::map_df(as_tibble) %>% 
  mutate(year=c("2021", "2020", "2019", "2018", "mean")) %>% 
  select(-annual) %>% 
  filter(year != "mean") %>% 
  tidyr::pivot_longer(
    january:december, names_to = "month", values_to = "solar_irradiation_jpercm2"
  ) %>% 
  mutate(solar_irradiation_jpercm2 = dplyr::na_if(solar_irradiation_jpercm2, ""),
         solar_irradiation_jpercm2 = dplyr::na_if(solar_irradiation_jpercm2, "n/a"),
         month = rep(1:12, 4),
         solar_irradiation_jpercm2 = as.numeric(solar_irradiation_jpercm2))

solar_data <- 
  dunsany_solar_radiation %>% 
  inner_join(monthly_solar_generation, by = c("year", "month"))

solar_data <- 
  solar_data %>% 
  filter(!is.na(solar_irradiation_jpercm2)) %>% 
  tidyr::unite("year_month", c(year, month), sep="-") %>% 
  mutate(year_month = lubridate::ym(year_month)) %>% 
  filter(year_month >= "2020-06-01")

solar_data <- 
  solar_data %>% 
  mutate(
    normalised_irradiation = solar_irradiation_jpercm2 / 
      max(solar_irradiation_jpercm2, na.rm = TRUE),
    normalised_solar_power = solar_power_generation / 
      max(solar_power_generation, na.rm = TRUE)
  ) %>% 
  arrange(year_month)

solar_data <- 
  solar_data %>% 
  mutate(is_training = !is.na(normalised_solar_power))

# Train

solar_model <- lm(
  solar_power_generation ~ solar_irradiation_jpercm2, 
  data = filter(solar_data, is_training, !is.na(solar_power_generation))
)
solar_model_confidence <- confint(solar_model)


# Calculate Estimates
solar_data <- 
  solar_data %>% 
  mutate(
    fitted_values = c(
      solar_model$fitted.values, 
      predict(solar_model, 
              dplyr::select(
                filter(solar_data, !is_training), 
                solar_irradiation_jpercm2
                )
              )
      ),
    lower_confint = ((solar_irradiation_jpercm2
                     * solar_model_confidence["solar_irradiation_jpercm2", ][[1]]) 
                     + solar_model$coefficients["(Intercept)"[[1]]]),
    upper_confint = ((solar_irradiation_jpercm2 
                     * solar_model_confidence["solar_irradiation_jpercm2", ][[2]])
                     + solar_model$coefficients["(Intercept)"[[1]]]),
    lower_confint_value = lower_confint * ELECTRICITY_PRICE * ESTIMATED_SELF_USE_RATE,
    upper_confint_value = upper_confint * ELECTRICITY_PRICE * ESTIMATED_SELF_USE_RATE,
    estimated_energy_value = fitted_values * ELECTRICITY_PRICE,
    estimated_energy_utilised_value = estimated_energy_value * ESTIMATED_SELF_USE_RATE,
    estimated_feed_in_tarrif_value = fitted_values * FEED_IN_TARRIF_PRICE * (1 - ESTIMATED_SELF_USE_RATE)
  )

# Push
push_message <- create_message(solar_data)

generation_plot <- 
  solar_data %>% 
  select(
    year_month, c(estimated_energy_utilised_value, estimated_feed_in_tarrif_value)
  ) %>% 
  tidyr::pivot_longer(
    cols = c(estimated_energy_utilised_value, estimated_feed_in_tarrif_value),
    names_to = "value_type",
    values_to = "euro"
  ) %>% 
  mutate(energy_type = factor(
    value_type, 
    levels = c("estimated_feed_in_tarrif_value", "estimated_energy_utilised_value"))
  ) %>% 
  ggplot(aes(
    year_month, 
    euro, 
    fill = forcats::fct_rev(value_type), 
    group = forcats::fct_rev(value_type)
  )) +
  geom_bar(size = 1.2, stat="identity") +
  theme_light() +
  theme(panel.grid.minor.x = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "top") +
  scale_fill_brewer(
    type = "qual", 
    palette = "Set2", 
    labels = c(
      paste0("Feed In Tarrif at ", 100 * FEED_IN_TARRIF_PRICE, "c"), "Utilised"
    )
  ) +
  labs(
    title = paste0("Estimated Solar Energy Value Realised - Feed In Tarrif & Utilised"),
    x = "Year-Month",
    y = "Euro",
    fill = "Estimated Value Per Month (Euro)"
  )

ggsave(
  "generation_plot.png", 
  plot = generation_plot,
  width = 12, 
  height = 6.75
)

RPushbullet::pbPost(
  "file",
  body = paste0(
    push_message$title_text,
    "\n\n",
    push_message$body_text
  ),
  url = "generation_plot.png",
  filetype = "image/png"
)