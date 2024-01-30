# GAM, hourly
here::i_am("R/gam.R")
source(here::here("R", "graphics.R"))
if (!exists("history_weather_county")){
  source(here::here("R", "init.R"))
}

# inspect one model in particular
my_hour <- 12

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
  "day_of_week",
  "eic_count",
  "installed_capacity",
  "target_lag_7"
)

wf <- workflow() %>%
  add_variables(outcomes = c(target), predictors = all_of(gam_predictors)) %>%
  add_model(
    spec,
    formula = target ~ s(temperature) +
    s(target_lag_7) +
    euros_per_mwh +
    # surface_pressure +
    # shortwave_radiation +
    direct_solar_radiation +
    cloudcover_total +
    installed_capacity +
    eic_count +
    s(dewpoint) +
    is_winter +
    day_of_week +
    is_business:day_of_week
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

fit_cons <- train_features_cons %>%
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


train_features_cons %>%
  filter(hour == my_hour) %>%
  ggplot(aes(x = eic_count, y = target)) +
  geom_point() +
  facet_grid(cols = vars(is_business))

train_features_cons %>%
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
test <- test_features %>%
  filter(is_consumption == 1) %>%
  select(datetime, hour, is_business, target, all_of(gam_predictors)) %>%
  group_by(hour) %>%
  nest()

prevision <- test %>%
  inner_join(fit_cons, by = "hour") %>%
  mutate(.pred = map2(fit, data, \(x, y) predict_hourly(model = x, df = y))) %>%
  unnest(c(data, .pred)) %>%
  select(datetime, is_business, target, .pred)

prevision %>%
  arrange(datetime) %>%
  rename(prev = .pred) %>%
  pivot_longer(
    names_to = "type",
    values_to = "puis_MW",
    cols = c("target", "prev")
  ) %>%
  # filter(datetime >= ymd("2023-02-01"), datetime <= ymd("2023-02-28")) %>%
  draw_target_vs_prev() +
  facet_grid(cols = vars(is_business))

### Metrics ###
get_baseline(
  yardstick::mape,
  test_pred_class = filter(prevision, hour == my_hour)
)
get_baseline(
  yardstick::rmse,
  test_pred_class = filter(prevision, hour == my_hour)
)
