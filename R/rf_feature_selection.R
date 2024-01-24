### Random Forest feature selection ###
ntree <- 100

history_weather_avg <- history_weather_county %>%
  group_by(datetime) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  select(-county)
train_weather <- train %>%
  filter(!prediction_unit_id %in% id_missing_values_in_train) %>%
  group_by(is_consumption, prediction_unit_id) %>%
  mutate(
    # interpolate NAs at daylight saving time
    target = na.approx(target),
    is_consumption = as.factor(is_consumption),
    is_business = as.factor(is_business),
    product_type = as.factor(product_type)
  ) %>%
  ungroup() %>%
  group_by(datetime, is_consumption) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>%
  inner_join(
    history_weather_avg,
    by = c("datetime"),
    multiple = "all"
  ) %>%
  ungroup() %>%
  select(
    -starts_with("data_block"),
    -row_id,
    -prediction_unit_id,
    -datetime,
    -county
  )

set.seed(4543)
tic("random forest fitting : consumption")
rf_fit <- randomForest(
  target ~ .,
  data = train_weather,
  ntree = ntree,
  keep.forest = FALSE,
  importance = TRUE
)
toc()

# scale problem ?
rf_fit$importance
