# GAM, hourly
here::i_am("R/gam.R")
source(here::here("R", "graphics.R"))
source(here::here("R", "utils.R"))
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
  "winddirection_10m",
  "windspeed_10m",
  "is_winter",
  "is_weekend",
  "day_of_week",
  "eic_count",
  "installed_capacity",
  "target_lag_5",
  "target_lag_6",
  "target_lag_7",
  "target_lag_8",
  "target_rollmean_7",
  "temp_smooth"
)
gam_predictors_norm <- c(
  "temperature",
  "euros_per_mwh",
  "surface_pressure",
  "shortwave_radiation",
  "direct_solar_radiation",
  "diffuse_radiation",
  "dewpoint",
  "cloudcover_low",
  "cloudcover_total",
  "winddirection_10m",
  "windspeed_10m",
  "is_winter",
  "is_weekend",
  "day_of_week",
  "target_normalized_lag_5",
  "target_normalized_lag_6",
  "target_normalized_lag_7",
  "target_normalized_lag_8",
  "target_normalized_rollmean_7",
  "temp_smooth"
)

wf <- workflow() %>%
  add_variables(outcomes = c(target), predictors = all_of(gam_predictors)) %>%
  add_model(
    spec,
    formula = target ~ s(temperature, k = 16) +
    s(temp_smooth) +
    target_lag_5 +
    target_lag_6 +
    target_lag_7 +
    target_lag_8 +
    s(target_rollmean_7) +
    # surface_pressure +
    # shortwave_radiation +
    direct_solar_radiation +
    cloudcover_total +
    installed_capacity +
    # eic_count +
    s(dewpoint, k = 12) +
    is_winter +
    day_of_week +
    is_weekend
  )

wf_norm <- workflow() %>%
  add_variables(
    outcomes = c(target_normalized),
    predictors = all_of(gam_predictors_norm)
  ) %>%
  add_model(
    spec,
    formula = target_normalized ~ s(temperature, k = 16) +
    # s(temp_smooth) +
    target_normalized_lag_5 +
    target_normalized_lag_6 +
    target_normalized_lag_7 +
    target_normalized_lag_8 +
    s(target_normalized_rollmean_7) +
    direct_solar_radiation +
    cloudcover_total +
    s(dewpoint, k = 12) +
    is_winter +
    day_of_week +
    is_weekend
  )

tic("Fitting GAM models by hour")
fit_cons <- train_features_cons %>%
  group_by(hour, product_type, is_business) %>%
  nest() %>%
  mutate(fit = map(data, \(data) fit_hourly(data, workflow = wf))) %>%
  select(hour, fit)
toc()

tic("Fitting GAM models by hour, target normalized by installed_capacity")
fit_cons <- train_features_cons %>%
  group_by(hour, product_type, is_business) %>%
  nest() %>%
  mutate(fit = map(data, \(data) fit_hourly(data, workflow = wf_norm))) %>%
  select(hour, fit)
toc()

model <- fit_cons %>%
  filter(hour == my_hour, product_type == 1, is_business == 0) %>%
  pull(fit)

# model summary
model[[1]] %>%
  extract_fit_engine() %>%
  summary()

# plot model
model[[1]] %>%
  extract_fit_engine() %>%
  plot(residuals = TRUE, shade = TRUE, pch = 1, cex = 1)

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
predictors <- gam_predictors_norm
target_name <- "target_normalized"
test <- test_features %>%
  filter(is_consumption == 1) %>%
  select(
    datetime,
    hour,
    is_business,
    product_type,
    eic_count,
    target,
    all_of(predictors)
  ) %>%
  group_by(hour, is_business, product_type) %>%
  nest()

prevision <- test %>%
  inner_join(fit_cons, by = c("hour", "product_type", "is_business")) %>%
  mutate(.pred = map2(fit, data, \(x, y) predict_hourly(model = x, df = y))) %>%
  unnest(c(data, .pred)) %>%
  select(
    datetime,
    is_business,
    product_type,
    eic_count,
    target,
    .pred
  ) %>%
  mutate(.pred = case_when(
      target_name == "target_normalized" ~ .pred * eic_count,
      .default = .pred
    )
  )

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
  facet_grid(
    rows = vars(product_type),
    cols = vars(is_business),
    scales = "free"
  )

### Metrics ###
get_baseline(
  yardstick::mae,
  test_pred_class = filter(prevision, hour == my_hour)
)
get_baseline(
  yardstick::mape,
  test_pred_class = filter(prevision, hour == my_hour)
)
get_baseline(
  yardstick::rmse,
  test_pred_class = filter(prevision, hour == my_hour)
)

## Whole test set
get_baseline(
  yardstick::mae,
  test_pred_class = ungroup(prevision)
)
get_baseline(
  yardstick::mape,
  test_pred_class = ungroup(prevision)
)
get_baseline(
  yardstick::rmse,
  test_pred_class = ungroup(prevision)
)