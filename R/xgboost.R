here::i_am("R/xgboost.R")
source(here::here("R", "libraries.R"))
source(here::here("R", "utils.R"))
if (!exists("ID_TO_COUNTY")){
  source(here::here("R", "load_dataset.R"))
}
if (!exists("history_weather_county")){
  source(here::here("R", "init.R"))
}


set.seed(42)
process_TRAIN()
numeric_split <- split_train_test(df = train_features)

classic_predictors <- c(
  "hour",
  "is_business",
  "product_type",
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
nesting_variables <- c("product_type", "is_business")
predictors_nested_model <- setdiff(classic_predictors, nesting_variables)

# classic_predictors OR predictors_nested_model
predictors <- classic_predictors
rec <- numeric_split$train %>%
  select(target, all_of(predictors)) %>%
  recipe() %>%
  update_role(all_of(predictors), new_role = "predictor") %>%
  update_role(target, new_role = "outcome") %>%
  step_zv() %>%
  step_dummy(all_nominal())
model_spec <-
  boost_tree(
    learn_rate = 0.1,
    tree_depth = 15,
    trees = 5000,
    stop_iter = 3
  ) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

wf_xgb <- workflow() %>%
  add_model(model_spec) %>%
  add_recipe(rec)

### MODELING ###
## Focusing on consumption

# Classical
tic("Fitting XGBoost model")
fit <- numeric_split$train %>%
  filter(is_consumption == 1) %>%
  fit(wf_xgb, data = .) %>%
  withr::with_seed(7, .)
toc()

# Nested
tic("Fitting XGBoost with nested data")
fit <- numeric_split$train %>%
  filter(is_consumption == 1) %>%
  group_by(across(all_of(nesting_variables))) %>%
  nest() %>%
  mutate(fit = map(data, \(data) fit_nested(df = data, wf = wf_xgb))) %>%
  select(all_of(nesting_variables), fit) %>%
  withr::with_seed(7, .)
toc()

### PREDICTIONS ###

# Classical
prevision <- numeric_split$test %>%
  filter(is_consumption == 1) %>%
  augment(fit, .)

get_baseline(yardstick::mae, prevision)
get_baseline(yardstick::mape, prevision)
get_baseline(yardstick::rmse, prevision)

# Nested
prevision <- numeric_split$test %>%
  filter(is_consumption == 1) %>%
  select(datetime, target, all_of(nesting_variables), all_of(predictors)) %>%
  group_by(across(all_of(nesting_variables))) %>%
  nest() %>%
  inner_join(fit, by = nesting_variables) %>%
  mutate(.pred = map2(
    fit,
    data,
    \(fit, data) predict_nested(model = fit, df = data)
  )) %>%
  unnest(c(data, .pred)) %>%
  select(datetime, all_of(nesting_variables), target, .pred)

get_baseline(yardstick::mae, ungroup(prevision))
get_baseline(yardstick::rmse, ungroup(prevision))


# results for default 15 trees
# 1 mae     standard        250.
# 1 rmse    standard        830.