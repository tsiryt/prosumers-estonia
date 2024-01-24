library(here)
here::i_am("init.R")

source(here::here("R", "libraries.R"))

client <- read_delim(here::here("data", "client.csv"))
train <- read_delim(here::here("data", "train.csv"))
gas_prices <- read_delim(here::here("data", "gas_prices.csv"))
electricity_prices <- read_delim(here::here("data", "electricity_prices.csv"))
# forecast_weather <- read_delim(here::here("data", "forecast_weather.csv"))
historical_weather <- read_delim(here::here("data", "historical_weather.csv"))
weather_station_to_county_mapping <- read_delim(here::here("data", "weather_station_to_county_mapping.csv")) %>%
  mutate(longitude = round(longitude, 1), latitude = round(latitude, 1))
id_to_county <- fromJSON(here::here("data", "county_id_to_name_map.json")) %>%
  unlist() %>%
  tibble::enframe() %>%
  rename(id = name, county = value)

# mapping prediction unit id
date_all_prod_units <- train %>%
  filter(is_consumption == 1) %>%
  group_by(datetime) %>%
  summarise(count = n()) %>%
  filter(count == max(count)) %>%
  pull(datetime) %>%
  max()
ref_prediction_unit_id <- train %>%
  filter(datetime == date_all_prod_units, is_consumption == 1) %>%
  select(prediction_unit_id, county, is_business, product_type)

# IDs with missing train values
id_missing_values_in_train <- train %>%
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
