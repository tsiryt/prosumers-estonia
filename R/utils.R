#Convert date and time data into variables
# Convert date and time column to POSIXct type
# Calculate by column
process_TRAIN <- function(){
  TRAIN$datetime <- ymd_hms(TRAIN$datetime)
  TRAIN$year <- year(TRAIN$datetime)
  TRAIN$month <- month(TRAIN$datetime)
  TRAIN$day <- day(TRAIN$datetime)
  TRAIN$week_of_month <- as.character(week(floor_date(TRAIN$datetime, unit = "week")))
  TRAIN$weekday <- weekdays(TRAIN$datetime)
}

split_train_test <- function(df){
  #train test split
  split <- initial_split(df, prop = 0.8)
  train <- training(split)
  test <- testing(split)

  train <- train[complete.cases(train), ]

  # store result in a named list
  numeric_split <- list(train, test)
  names(numeric_split) <- c("train", "test")

  return(numeric_split)
}

## TODO ## : add model params as this function's params
get_model_workflow <- function(numeric_split){
  #data processing : remove variables that contain only a single value
  rec <- recipe(target ~ ., data = numeric_split$train) %>%
    step_zv()

  # model default 15 trees
  rule <-
    boost_tree(
      learn_rate = 0.01,
      tree_depth = 1000,
      trees = 15,
      stop_iter = 3
    ) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  # ML workflow
  wf <- 
    workflow() %>%
    add_model(rule) %>%
    add_recipe(rec)

  return(wf)
}

get_baseline <- function(metric, test_pred_class){
  info_baseline <- test_pred_class %>%
  metric(truth = target, estimate = .pred)
  return(info_baseline)
}

#' Fit a workflow on nested data. Call with purrr::map
#'
#' @param df A nested dataframe, or something coercible to one.
#' @returns A workflow model.
#' @examples
#' source(here::here("R", "init.R"))
#' fit_cons <- train_features_cons %>%
#'   group_by(hour) %>%
#'   nest() %>%
#'   mutate(fit = map(data, \(data) fit_nested(data, workflow = wf_norm))) %>%
#'   select(hour, fit)
fit_nested <- function(df, wf) {
  fit_wf <-
    wf %>%
    fit(data = df)

  return(fit_wf)
}

#' predict using nested data. Call with purrr::map
#'
#' @param model A workflow model fit.
#' @param df A dataframe, or something coercible to one.
#' @returns A vector of predictions.
#' @examples
#' source(here::here("R", "init.R"))
#' test <- test_features %>%
#'   filter(is_consumption == 1) %>%
#'   select(datetime, hour, is_business, target, all_of(gam_predictors)) %>%
#'   group_by(hour) %>%
#'   nest()
#' 
#' prevision <- test %>%
#'   inner_join(fit_cons, by = "hour") %>%
#'   mutate(.pred = map2(fit, data, \(x, y) predict_nested(model = x, df = y))) %>%
#'   unnest(c(data, .pred)) %>%
#'   select(datetime, is_business, product_type, target, .pred)
predict_nested <- function(model, df) {
  prev <- augment(model, df)
  return(prev$.pred)
}
