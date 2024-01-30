here::i_am("R/xgboost.R")
source(here::here("R", "libraries.R"))
if (!exists("ID_TO_COUNTY")){
  source(here::here("R", "load_dataset.R"))
}
if (!exists("history_weather_county")){
  source(here::here("R", "init.R"))
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