library(dplyr)
library(tidyr)
library(jsonlite)

ELECTRICITY_PRICE = 0.20
ESTIMATED_SELF_USE_RATE = 0.75

report <- jsonlite::fromJSON("https://prodapi.metweb.ie/monthly-data/Dunsany%20(Grange)")

monthly_solar_generation <- dplyr::tibble(
  year = c(rep("2021", 12), rep("2020", 12), rep("2019", 12), rep("2018", 12)),
  month = rep(1:12, 4),
  solar_power_generation = c(
    c(85, 157.5, 44.5, rep(NA, 9)),
    c(rep(NA, 5), 358.4, 394.9, 314.3, 332.8, 235, 119.6, 38.7),
    rep(NA, 12),
    rep(NA, 12)
  )
)

dunsany_solar_radiation <- 
  report$solar_radiation$report %>% 
  purrr::map_df(as_tibble) %>% 
  mutate(year=c("2021", "2020", "2019", "2018", "mean")) %>% 
  select(-annual) %>% 
  filter(year != "mean") %>% 
  tidyr::pivot_longer(january:december, names_to = "month", values_to = "solar_irradiation_jpercm2") %>% 
  mutate(solar_irradiation_jpercm2 = dplyr::na_if(solar_irradiation_jpercm2, ""),
         solar_irradiation_jpercm2 = dplyr::na_if(solar_irradiation_jpercm2, "n/a"),
         month = rep(1:12, 4),
         solar_irradiation_jpercm2 = as.numeric(solar_irradiation_jpercm2))

dunsany_solar_radiation <- 
  dunsany_solar_radiation %>% 
  inner_join(monthly_solar_generation, by = c("year", "month"))

reduced_dunsany_solar_radiation <- 
  dunsany_solar_radiation%>% 
  filter(!is.na(solar_irradiation_jpercm2),
         !is.na(solar_power_generation))

reduced_dunsany_solar_radiation <- 
  reduced_dunsany_solar_radiation %>% 
  mutate(month = sprintf("%02d", as.numeric(month))) %>% 
  mutate(year = as.integer(year)) %>% 
  tidyr::unite("year_month", year, month, sep="-", remove=FALSE) %>% 
  mutate(
    normalised_irradiation = solar_irradiation_jpercm2 / max(solar_irradiation_jpercm2),
    normalised_solar_power = solar_power_generation / max(solar_power_generation)
  ) %>% 
  arrange(year_month)

training_df <- 
  reduced_dunsany_solar_radiation %>% 
  filter(year_month != max(year_month))

current_df <-  
  reduced_dunsany_solar_radiation %>% 
  filter(year_month == max(year_month))

solar_model <- lm(
  solar_power_generation ~ solar_irradiation_jpercm2, 
  data = training_df
)

solar_model_confidence <- confint(solar_model)

current_month_values <- 
  list(
    lower_estimate_energy_produced = (current_df$solar_irradiation_jpercm2 * solar_model_confidence["solar_irradiation_jpercm2", ][[1]]),
    upper_estimate_energy_produced = (current_df$solar_irradiation_jpercm2 * solar_model_confidence["solar_irradiation_jpercm2", ][[2]])
  ) %>% 
  purrr::map_dbl(function(x) x + solar_model$coefficients["(Intercept)"][[1]])

current_month_values[["estimated_energy_produced"]] <- 
  predict(solar_model, dplyr::select(current_df, solar_irradiation_jpercm2))

current_month_values[["solar_irradiance"]] <- 
  current_df$solar_irradiation_jpercm2[[1]]

current_month_values[["estimated_energy_total_value"]] <- 
  current_month_values[["estimated_energy_produced"]] * ELECTRICITY_PRICE

current_month_values[["estimated_energy_utilised_value"]] <- 
  current_month_values[["estimated_energy_total_value"]] * ESTIMATED_SELF_USE_RATE

RPushbullet::pbPost(
  "note",
  title = paste0(
    "Solar value estimate for ", current_df$year_month, " so far: €", 
    round(current_month_values[["estimated_energy_utilised_value"]], 2)
  ),
  body = "Solar!"
)