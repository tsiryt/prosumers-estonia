library(here)
here::i_am("model.R")

source(here::here("init.R"))
client <- read_delim(here::here("data", "client.csv"))
train <- read_delim(here::here("data", "train.csv"))
gas_prices <- read_delim(here::here("data", "gas_prices.csv"))
electricity_prices <- read_delim(here::here("data", "electricity_prices.csv"))
forecast_weather <- read_delim(here::here("data", "forecast_weather.csv"))
id_to_county <- fromJSON(here::here("data", "county_id_to_name_map.json")) %>%
  unlist() %>%
  tibble::enframe() %>%
  rename(id = name, county = value)


#Convert date and time data into variables
library(lubridate)
# Convert date and time column to POSIXct type
train$datetime <- ymd_hms(train$datetime)
# Calculate by column
train$year <- year(train$datetime)
train$month <- month(train$datetime)
train$day <- day(train$datetime)
train$week_of_month <- as.character(week(floor_date(train$datetime, unit = "week")))
train$weekday <- weekdays(train$datetime)


set.seed(42)
#train test split
dai_split<- initial_split(train,prop=0.8)
train_dai<-training(dai_split)
test_dai<-testing(dai_split)

numeric_train_dai <- train_dai %>% 
  select(where(is.numeric)) %>%
  filter(!prediction_unit_id %in% id_na_in_train)

numeric_train_dai <- numeric_train_dai[complete.cases(numeric_train_dai), ]

numeric_test_dai <- test_dai %>% 
  select(where(is.numeric))

#data processing : remove variables that contain only a single value
rec <- recipe(target ~ ., data = numeric_train_dai ) %>%
  step_zv()

# model
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
  withr::with_seed(7)

#check missing values too
inspect_na(numeric_train_dai) %>% show_plot()
# summarize numerical columns
inspect_num(numeric_train_dai) %>% show_plot()
## correlation between columns
inspect_cor(numeric_train_dai) %>% show_plot()