# global variables

here::i_am("load_dataset.R")

CLIENT <- read_delim(here::here("data", "client.csv"))
TRAIN <- read_delim(here::here("data", "train.csv"))
GAS_PRICES <- read_delim(here::here("data", "gas_prices.csv"))
ELECTRICITY_PRICES <- read_delim(here::here("data", "electricity_prices.csv"))
FORECAST_WEATHER <- read_delim(here::here("data", "forecast_weather.csv"))
ID_TO_COUNTY <- fromJSON(here::here("data", "county_id_to_name_map.json")) %>%
    unlist() %>%
    tibble::enframe() %>%
    rename(id = name, county = value)

# useful regex : (GLOBAL_VAR)(?!\w)