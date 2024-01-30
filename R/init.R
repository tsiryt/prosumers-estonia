here::i_am("R/init.R")

source(here::here("R", "libraries.R"))
if (!exists("ID_TO_COUNTY")){
  source(here::here("R", "load_dataset.R"))
}

start_date <- min(TRAIN$datetime)

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
history_weather_county <- WEATHER_STATION_TO_COUNTY_MAPPING %>%
  filter(!is.na(county)) %>%
  inner_join(
    HISTORICAL_WEATHER,
    by = c("latitude", "longitude"), multiple = "all"
  ) %>%
  group_by(county, datetime) %>%
  summarise(across(where(is.numeric), mean)) %>%
  select(-longitude, -latitude)
# why rm ?
# rm(list = c("historical_weather"))

# splitting train & test sets
datetime_split <- ymd_hms("2023-01-24 00:00:00")

history_weather_avg <- history_weather_county %>%
  group_by(datetime) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  select(-county)
train_weather <- TRAIN %>%
  filter(
    !prediction_unit_id %in% id_missing_values_in_train,
    datetime <= datetime_split
  ) %>%
  group_by(is_consumption, prediction_unit_id) %>%
  mutate(
    # interpolate NAs at daylight saving time
    target = na.approx(target),
    is_consumption = as.factor(is_consumption),
    is_business = as.factor(is_business),
    product_type = as.factor(product_type),
    hour = hour(datetime)
  ) %>%
  ungroup() %>%
  group_by(datetime, is_consumption, is_business) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  inner_join(
    history_weather_avg,
    by = c("datetime"),
    multiple = "all"
  ) %>%
  ungroup() %>%
  select(
    -starts_with("data_block"),
    -row_id,
    -prediction_unit_id,
    # -datetime,
    -county
  )

train_weather_prod <- train_weather %>%
  filter(is_consumption == 0)
train_weather_cons <- train_weather %>%
  filter(is_consumption == 1)


all_features <- TRAIN %>%
  filter(
    !prediction_unit_id %in% id_missing_values_in_train
  ) %>%
  mutate(
    # interpolate NAs at daylight saving time
    target = na.approx(target),
    is_consumption = as.factor(is_consumption),
    county = as.factor(county),
    product_type = as.factor(product_type),
    is_business = as.factor(is_business),
    hour = hour(datetime),
    day_of_week = as.factor(wday(datetime)),
    is_weekend = as.factor(if_else(day_of_week %in% c(1, 7), 1, 0)),
    is_winter = as.factor(if_else(month(datetime) %in% 3:10, 0, 1)),
    date = as_date(datetime)
  ) %>%
  inner_join(
    CLIENT,
    by = c("date", "product_type", "county", "is_business")
  ) %>%
  group_by(
    datetime,
    hour,
    is_consumption,
    is_business,
    is_winter,
    is_weekend,
    product_type,
    day_of_week
  ) %>%
  summarise(
    target = sum(target, na.rm = TRUE),
    eic_count = max(eic_count),
    installed_capacity = max(installed_capacity)
  ) %>%
  ungroup() %>%
  group_by(hour, is_consumption, is_business, product_type) %>%
  mutate(target_lag_7 = dplyr::lag(target, 7)) %>%
  ungroup() %>%
  filter(datetime >= start_date + days(7)) %>%
  inner_join(
    history_weather_avg,
    by = c("datetime"),
    multiple = "all"
  ) %>%
  inner_join(
    ELECTRICITY_PRICES,
    by = c("datetime" = "forecast_date"),
    multiple = "all"
  ) %>%
  ungroup() %>%
  select(
    -starts_with("data_block"),
    -origin_date
  )

train_features <- all_features %>%
  filter(datetime < datetime_split)
test_features <- all_features %>%
  filter(datetime >= datetime_split)

train_features_prod <- train_features %>%
  filter(is_consumption == 0)
train_features_cons <- train_features %>%
  filter(is_consumption == 1)
