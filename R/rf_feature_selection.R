### Random Forest feature selection ###
# here::i_am("R/rf_feature_selection.R")
source(here::here("init.R"))

ntree <- 400

history_weather_avg <- history_weather_county %>%
  group_by(datetime) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE))) %>%
  select(-county)
train_weather <- TRAIN %>%
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

train_weather_prod <- train_weather %>%
  filter(is_consumption == 0)
train_weather_cons <- train_weather %>%
  filter(is_consumption == 1)


set.seed(4543)
tic("random forest fitting : consumption")
rf_fit_cons <- randomForest(
  target ~ .,
  data = train_weather_cons,
  ntree = ntree,
  keep.forest = TRUE,
  importance = TRUE
)
toc()

tic("random forest fitting : production")
rf_fit_prod <- randomForest(
  target ~ .,
  data = train_weather_prod,
  ntree = ntree,
  keep.forest = TRUE,
  importance = TRUE
)
toc()

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- rf_fit_prod$importance %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var.Names") %>%
  as_tibble()

ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(
    aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`),
    color = "skyblue"
  ) +
  geom_point(aes(size = IncNodePurity), color = "blue", alpha = 0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

# Partial effects
df_train <- data.frame(train_weather)
partialPlot(rf_fit_prod, df_train, shortwave_radiation)
