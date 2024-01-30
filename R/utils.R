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

split_TRAIN <- function(){
  #train test split
  split<- initial_split(TRAIN,prop=0.8)
  train<-training(split)
  test<-testing(split)

  train <- train %>% 
    select(where(is.numeric)) %>%
    filter(!prediction_unit_id %in% id_missing_values_in_train)

  train <- train[complete.cases(train), ]

  test <- test %>% 
    select(where(is.numeric))
  # store result in a named list
  numeric_split <- list(train, test)
  names(numeric_split) <- c("train", "test")

  return(numeric_split)
}

get_model_workflow <- function(numeric_split){
  #data processing : remove variables that contain only a single value
  rec <- recipe(target ~ ., data = numeric_split$train) %>%
    step_zv()

  # model default 15 trees
  rule <- boost_tree(learn_rate = 0.01, tree_depth = 1000, trees = 15, stop_iter = 3) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  # ML workflow
  wf <- workflow() %>% 
    add_model(rule) %>% 
    add_recipe(rec)

  return(wf)
}

get_baseline <- function(metric, test_pred_class){
  info_baseline <- test_pred_class %>%
  metric(truth = target, estimate = .pred)
  return(info_baseline)
}

#' Fit a model on nested data. Call with purrr::map
#'
#' @param df A dataframe, or something coercible to one.
#' @returns A parsnip model fit.
#' @examples
#' source(here::here("R", "init.R"))
#' fit_cons <- train_features_cons %>%
#'   group_by(hour) %>%
#'   nest() %>%
#'   mutate(fit = map(data, fit_hourly)) %>%
#'   select(hour, fit)
fit_hourly <- function(df) {
  #fit workflow on train data
  fit_wf <-
    wf %>%
    fit(data = df) %>%
    extract_fit_parsnip()

  return(fit_wf)
}

#' predict using nested data. Call with purrr::map
#'
#' @param model A parsnip model fit.
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
#'   mutate(.pred = map2(fit, data, \(x, y) predict_hourly(model = x, df = y))) %>%
#'   unnest(c(data, .pred)) %>%
#'   select(datetime, is_business, product_type, target, .pred)
predict_hourly <- function(model, df) {
  prev <- augment(model, df)
  return(prev$.pred)
}