library(dplyr)
library(sf)
library(ggplot2)
library(jmastats)

# 1. Data collect ----------------------------------------------------
# remotes::install_git("https://gitlab.com/uribo/jmastats")

if (rlang::is_false(file.exists(here::here("data-raw/weather.rds")))) {
  if (rlang::is_false(dir.exists(here::here("data-raw"))))
    dir.create(here::here("data-raw"))
  data("stations", package = "jmastats")
  # Access to JMA website
  df_weather_20180815 <-
    stations %>%
    filter(station_type == "四") %>%
    select(station_no, station_type, station_name, elevation, pref_code, block_no) %>%
    mutate(
      weather = purrr::map(block_no,
                           ~ jma_collect("hourly",
                                         block_no = .x,
                                         year = 2018,
                                         month = 8,
                                         day = 15) %>%
                             dplyr::select(date,
                                           time,
                                           contains("precipitation"),
                                           contains("temperature"))
      )) %>% 
    mutate(weather_summary = purrr::map(weather,
                                        ~ .x %>% 
                                          jmastats::parse_unit() %>%
                                          mutate_at(vars(c("precipitation_mm", "temperature")),
                                                    funs(as.numeric)) %>% 
                                          summarise(
                                            precipitation_sum = sum(precipitation_mm, na.rm = TRUE),
                                            temperature_mean = mean(temperature, na.rm = TRUE),
                                            temperature_max = max(temperature, na.rm = TRUE),
                                            temperature_min = min(temperature, na.rm = TRUE)
                                          ))) %>% 
    tidyr::unnest(weather_summary) %>% 
    mutate(rainy = if_else(precipitation_sum > 0.1, TRUE, FALSE)) %>% 
    filter(!is.nan(temperature_mean)) %>% 
    select(station_no,
           elevation, 
           starts_with("temperature"), 
           precipitation_sum, rainy)
  readr::write_rds(df_weather_20180815, 
                   here::here("data-raw/weather.rds"))  
}
