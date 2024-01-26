# here::i_am("init.R")

source(here::here("R", "libraries.R"))
source(here::here("load_dataset.R"))

# mapping prediction unit id
date_all_prod_units <- TRAIN %>%
  filter(is_consumption == 1) %>% #consumers
  group_by(datetime) %>%
  summarise(count = n()) %>%
  filter(count == max(count)) %>%
  pull(datetime) %>%
  max()
ref_prediction_unit_id <- TRAIN %>%
  filter(datetime == date_all_prod_units, is_consumption == 1) %>%
  select(prediction_unit_id, county, is_business, product_type)

# IDs with missing train values
id_missing_values_in_train <- TRAIN %>%
  group_by(prediction_unit_id, is_consumption) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < max(count)) %>%
  pull(prediction_unit_id) %>%
  unique()

# weather data by county
history_weather_county <- weather_station_to_county_mapping %>%
  filter(!is.na(county)) %>%
  inner_join(
    historical_weather,
    by = c("latitude", "longitude"), multiple = "all"
  ) %>%
  group_by(county, datetime) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(-longitude, -latitude)

rm(list = c("historical_weather"))
