if (!exists("ID_TO_COUNTY")){
  source(here::here("R", "load_dataset.R"))
}
if (!exists("history_weather_county")){
  source(here::here("R" ,"init.R"))
}

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
    mutate(target = na.approx(target))
  # store result in a named list
  numeric_split <- list(train, test)
  names(numeric_split) <- c("train", "test")

  return(numeric_split)
}
