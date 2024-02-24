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
engine <- "lightgbm"
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
    learn_rate = tune(),
    tree_depth = 15,
    trees = 5000,
    # xgboost gamma
    loss_reduction = NULL,
    stop_iter = 3
  ) %>%
  set_engine(engine) %>%
  set_mode("regression")

### TUNING ###

## Learning rate
# Tuning learning rate with 10-fold CV took 50mn
# Grid search
lr_grid <- grid_regular(learn_rate(), levels = 5)
model_spec_lr <-
  boost_tree(
    learn_rate = tune(),
    tree_depth = 15,
    trees = 5000,
    stop_iter = 3
  ) %>%
  set_engine(engine) %>%
  set_mode("regression")
train_folds <- numeric_split$train %>%
  vfold_cv(v = 5) %>%
  withr::with_seed(234, .)

wf_xgb_lr <- workflow() %>%
  add_model(model_spec_lr) %>%
  add_recipe(rec)

tic(glue("Tuning learn rate using {engine}"))
lr_res <-
  wf_xgb_lr %>%
  tune_grid(
    resamples = train_folds,
    grid = lr_grid,
    metrics = metric_set(rmse, mae, mape)
)
toc()

lr_res %>%
  collect_metrics() %>%
  ggplot(aes(x = learn_rate, y = mean)) +
  geom_line() +
  facet_wrap(~ .metric, scales = "free")
# best was learn_rate = 0.1
best_learn_rate <- select_best(lr_res, "mae")

wf_lr_tuned <-
  wf_xgb_lr %>%
  finalize_workflow(best_learn_rate)

# Tree hyperparams
tree_grid <- grid_regular(min_n(), tree_depth(), levels = 3)
model_spec_tree <-
  boost_tree(
    learn_rate = 0.1,
    tree_depth = tune(),
    min_n = tune(),
    trees = 5000,
    stop_iter = 3
  ) %>%
  set_engine(engine) %>%
  set_mode("regression")
train_folds <- numeric_split$train %>%
  vfold_cv(v = 5) %>%
  withr::with_seed(61, .)

wf_xgb_tree <- workflow() %>%
  add_model(model_spec_tree) %>%
  add_recipe(rec)

# Tuning took 30 mins with 5-fold cv and 9 grid combos
tic(glue("Tuning tree params using {engine}"))
tree_res <-
  wf_xgb_tree %>%
  tune_grid(
    resamples = train_folds,
    grid = tree_grid,
    metrics = metric_set(rmse, mae)
)
toc()

tree_res %>%
  collect_metrics() %>%
  mutate(min_n = as.factor(min_n)) %>%
  ggplot(aes(x = tree_depth, y = mean)) +
  geom_point(size = 2, aes(col = min_n)) +
  facet_wrap(~ .metric, scales = "free")
# best was min_n = 40, tree_depth = 15
best_tree <- select_best(tree_res, "rmse")

wf_xgb <-
  wf_xgb_tree %>%
  finalize_workflow(best_tree)

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
get_baseline(yardstick::mape, ungroup(prevision))
get_baseline(yardstick::rmse, ungroup(prevision))


# results for not normalised, default hyperparams
# mae unnested 167
# rmse unnested 412

# after tuning,, mape for nested is far better than unnested (6,57 vs 19.2)