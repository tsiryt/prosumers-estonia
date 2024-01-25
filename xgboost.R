here::i_am("model.R")

source(here::here("load_dataset.R"))
source(here::here("init.R"))

#Convert date and time data into variables
# Convert date and time column to POSIXct type
# Calculate by column
process_TRAIN <- process_TRAIN(){
  TRAIN$datetime <- ymd_hms(TRAIN$datetime)
  TRAIN$year <- year(TRAIN$datetime)
  TRAIN$month <- month(TRAIN$datetime)
  TRAIN$day <- day(TRAIN$datetime)
  TRAIN$week_of_month <- as.character(week(floor_date(TRAIN$datetime, unit = "week")))
  TRAIN$weekday <- weekdays(TRAIN$datetime)
}



set.seed(42)
#train test split
dai_split<- initial_split(TRAIN,prop=0.8)
train_dai<-training(dai_split)
test_dai<-testing(dai_split)

numeric_train_dai <- train_dai %>% 
  select(where(is.numeric)) %>%
  filter(!prediction_unit_id %in% id_na_in_train)

numeric_train_dai <- numeric_train_dai[complete.cases(numeric_train_dai), ]

numeric_test_dai <- test_dai %>% 
  select(where(is.numeric))

#data processing : remove variables that contain only a single value
rec <- recipe(target ~ ., data = numeric_train_dai) %>%
  step_zv()

# model default 15 trees
rule <- boost_tree(learn_rate = 0.01, tree_depth = 1000) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# ML workflow
wf <- workflow() %>% 
  add_model(rule) %>% 
  add_recipe(rec)

# modeling
model <- wf %>% 
  fit(data = numeric_train_dai[complete.cases(numeric_train_dai), ]) %>% 
  withr::with_seed(7, .)

# #check missing values too
# inspect_na(numeric_train_dai) %>% show_plot()
# # summarize numerical columns
# inspect_num(numeric_train_dai) %>% show_plot()
# ## correlation between columns
# inspect_cor(numeric_train_dai) %>% show_plot()

test_pred_class <-
  bind_cols(
    test_dai,
    predict(model, new_data = test_dai, type = "numeric")
  ) %>%
  select(target, .pred, everything())

info_baseline <-
  test_pred_class %>%
  mae(truth = target, estimate = .pred)
# 1 mae     standard        250. 
info_baseline

info_baseline <-
  test_pred_class %>%
  rmse(truth = target, estimate = .pred)
# 1 rmse    standard        830.
info_baseline