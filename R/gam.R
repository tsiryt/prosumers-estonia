# GAM par heure
here::i_am("R/gam.R")
source(here::here("R", "graphics.R"))
if (!exists("history_weather_county")){
  source(here::here("R", "init.R"))
}

my_hour <- 12

train_weather_price <- TRAIN %>%
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
    hour = hour(datetime),
    day_of_week = as.factor(wday(datetime)),
    is_winter = as.factor(if_else(month(datetime) %in% 3:10, 0, 1))
  ) %>%
  ungroup() %>%
  group_by(datetime, is_consumption, is_business, is_winter, day_of_week) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
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
    -row_id,
    -prediction_unit_id,
    # -datetime,
    -county
  )

train_weather_price_prod <- train_weather_price %>%
  filter(is_consumption == 0)
train_weather_price_cons <- train_weather_price %>%
  filter(is_consumption == 1)

spec <-
  gen_additive_mod(select_features = FALSE) %>%
  set_engine(
    "mgcv",
    method = "REML",
    # residuals were heavily right-skewed, particularly at lower values
    family = Gamma(link = "log")
  ) %>%
  set_mode("regression")
gam_predictors <- c(
  "temperature",
  "euros_per_mwh",
  "surface_pressure",
  "shortwave_radiation",
  "direct_solar_radiation",
  "diffuse_radiation",
  "dewpoint",
  "cloudcover_low",
  "cloudcover_total",
  "is_business",
  "winddirection_10m",
  "windspeed_10m",
  "is_winter",
  "day_of_week"
)

wf <- workflow() %>%
  add_variables(outcomes = c(target), predictors = all_of(gam_predictors)) %>%
  add_model(
    spec,
    formula = target ~ s(temperature) +
    euros_per_mwh +
    # surface_pressure +
    # shortwave_radiation +
    direct_solar_radiation +
    s(cloudcover_total) +
    s(dewpoint) +
    is_winter +
    day_of_week
  )

# fit a model on nested data. Call with purrr::map
fit_hourly <- function(df) {
  #fit workflow on train data
  fit_wf <-
    wf %>%
    fit(data = df) %>%
    extract_fit_parsnip()

  return(fit_wf)
}

# predict using nested data. Call with purrr::map
predict_hourly <- function(model, df) {
  prev <- augment(model, df)
  return(prev$.pred)
}

fit_cons <- train_weather_price_cons %>%
  filter(is_business == 0) %>%
  group_by(hour) %>%
  nest() %>%
  mutate(fit = map(data, fit_hourly)) %>%
  select(hour, fit)
model <- fit_cons %>%
  filter(hour == my_hour) %>%
  pull(fit)

# model summary
model[[1]] %>%
  extract_fit_engine() %>%
  summary()

# plot model
model[[1]] %>%
  extract_fit_engine() %>%
  plot()

# concurvity
model[[1]] %>%
  extract_fit_engine() %>%
  concurvity()

# check residuals
model[[1]] %>%
  extract_fit_engine() %>%
  gam.check()


train_weather_price_cons %>%
  filter(hour == my_hour) %>%
  ggplot(aes(x = euros_per_mwh, y = target)) +
  geom_point()

train_weather_price_cons %>%
  mutate(weird = if_else(target <= 55, "weird", "normal")) %>%
  filter(hour == my_hour, is_business == 0) %>%
  select(datetime, target, weird) %>%
  ggplot(aes(x = datetime, y = target)) +
  geom_point(aes(col = weird)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d")

# weird line : plotting response VS fitted (when using identity as link)
model[[1]] %>%
  extract_fit_engine() %>%
  augment() %>%
  ggplot(aes(x = .fitted, y = target)) +
  geom_point(aes(color = is_winter))

model[[1]] %>%
  extract_fit_engine() %>%
  augment()
  
train_weather_price_cons %>%
  filter(hour == 12) %>%
  select(datetime, target, is_business, euros_per_mwh) %>%
  pivot_longer(names_to = "feature", values_to = "value", cols = c("target", "euros_per_mwh")) %>%
  ggplot(aes(x = datetime, y = value)) +
  geom_line(aes(col = feature)) +
  facet_grid(cols = vars(is_business))


### PREDICT ### -------------
test_weather_price_cons <- TRAIN %>%
  filter(
    !prediction_unit_id %in% id_missing_values_in_train,
    # for testing purposes
    is_business == 0,
    is_consumption == 1,
    datetime >= datetime_split
  ) %>%
  group_by(is_consumption, prediction_unit_id) %>%
  mutate(
    # interpolate NAs at daylight saving time
    target = na.approx(target),
    is_consumption = as.factor(is_consumption),
    is_business = as.factor(is_business),
    product_type = as.factor(product_type),
    hour = hour(datetime),
    day_of_week = as.factor(wday(datetime)),
    is_winter = as.factor(if_else(month(datetime) %in% 3:10, 0, 1))
  ) %>%
  ungroup() %>%
  group_by(datetime, hour, is_consumption, is_business, is_winter, day_of_week) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  inner_join(
    history_weather_avg,
    by = c("datetime"),
    multiple = "all"
  ) %>%
  ungroup() %>%
  inner_join(
    ELECTRICITY_PRICES,
    by = c("datetime" = "forecast_date"),
    multiple = "all"
  ) %>%
  ungroup() %>%
  select(datetime, hour, is_business, target, all_of(gam_predictors)) %>%
  group_by(hour) %>%
  nest()

prevision <- test_weather_price_cons %>%
  inner_join(fit_cons, by = "hour") %>%
  mutate(.pred = map2(fit, data, \(x, y) predict_hourly(model = x, df = y))) %>%
  unnest(c(data, .pred)) %>%
  select(datetime, target, .pred)

prevision %>%
  arrange(datetime) %>%
  rename(prev = .pred) %>%
  pivot_longer(
    names_to = "type",
    values_to = "puis_MW",
    cols = c("target", "prev")
  ) %>%
  # filter(datetime >= ymd("2023-02-01"), datetime <= ymd("2023-02-28")) %>%
  draw_target_vs_prev()

### Metrics ###
get_baseline(yardstick::mape, test_pred_class = my_df)
get_baseline(yardstick::rmse, test_pred_class = my_df)
