# global variables

here::i_am("R/load_dataset.R")

CLIENT <- read_delim(here::here("data", "client.csv")) %>%
  mutate(
    across(all_of(c("product_type", "county", "is_business")),
    ~ as.factor(.))
  )
TRAIN <- read_delim(here::here("data", "train.csv"))
GAS_PRICES <- read_delim(here::here("data", "gas_prices.csv"))
ELECTRICITY_PRICES <- read_delim(here::here("data", "electricity_prices.csv"))
FORECAST_WEATHER <- read_delim(here::here("data", "forecast_weather.csv"))
HISTORICAL_WEATHER <- read_delim(here::here("data", "historical_weather.csv"))
WEATHER_STATION_TO_COUNTY_MAPPING <- read_delim(here::here("data", "weather_station_to_county_mapping.csv")) %>%
  mutate(longitude = round(longitude, 1), latitude = round(latitude, 1))
ID_TO_COUNTY <- fromJSON(here::here("data", "county_id_to_name_map.json")) %>%
    unlist() %>%
    tibble::enframe() %>%
    rename(id = name, county = value)

# useful regex : (GLOBAL_VAR)(?!\w)