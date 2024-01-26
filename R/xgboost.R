if (!exists("ID_TO_COUNTY")){
  source(here::here("load_dataset.R"))
}
if (!exists("history_weather_county")){
  source(here::here("init.R"))
}


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

set.seed(42)
process_TRAIN()
numeric_split <- split_TRAIN()
wf <- get_model_workflow(numeric_split)
# modeling
model <- wf %>% 
  fit(data = numeric_split$train[complete.cases(numeric_split$train), ]) %>% 
  withr::with_seed(7, .)
# predictions
test_pred_class <-
  bind_cols(
    numeric_split$test,
    predict(model, new_data = numeric_split$test, type = "numeric")
  ) %>%
  select(target, .pred, everything())
get_baseline(yardstick::mae, test_pred_class)
get_baseline(yardstick::rmse, test_pred_class)

# results for default 15 trees
# 1 mae     standard        250. 
# 1 rmse    standard        830.