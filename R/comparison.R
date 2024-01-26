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


train_data <- numeric_split$train
test_data <- numeric_split$test

set.seed(42)
train_resamples <- bootstraps(train_data)

model_specs <- list(
  linear_reg = linear_reg(),
  decision_tree = decision_tree(),
  random_forest = rand_forest(),
  xgboost = boost_tree()
)

model_specs <- lapply(model_specs, function(model) {
  model %>% set_mode("regression")
})


all_workflows <- 
  workflow_set(
    preproc = list("formula" = target ~ .),
    models = model_specs
  )
# https://workflowsets.tidymodels.org/articles/tuning-and-comparing-models.html
all_workflows <- 
  all_workflows %>% 
  # Specifying arguments here adds to any previously set with `option_add()`:
  workflow_map(resamples = train_resamples, grid = 20, verbose = TRUE)

# all_workflows <- all_workflows %>%
#     filter(wflow_id != "formula_random_forest")

rank_results(all_workflows, rank_metric = "rmse")
autoplot(all_workflows, metric = "rmse")

test_predictions <- predict(all_workflows$formula_xgboost, new_data = test_data)