### Random Forest feature selection ###
here::i_am("R/rf_feature_selection.R")
if (!exists("history_weather_county")){
  source(here::here("R", "init.R"))
}

ntree <- 400

set.seed(4543)
tic("random forest fitting : consumption")
rf_fit_cons <- randomForest(
  target ~ .,
  data = train_features_cons,
  ntree = ntree,
  keep.forest = TRUE,
  importance = TRUE
)
toc()

tic("random forest fitting : production")
rf_fit_prod <- randomForest(
  target ~ .,
  data = train_features_prod,
  ntree = ntree,
  keep.forest = TRUE,
  importance = TRUE
)
toc()

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- rf_fit_cons$importance %>%
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
partialPlot(rf_fit_cons, df_train, surface_pressure)

# prod : shortwave_radiation, direct_solar_radiation, diffuse_radiation, dewpoint, cloudcover_total, surface_pressure, temperature
# cons : temperature, surface_pressure, shortwave_radiation, direct_solar_radiation, diffuse_radiation, dewpoint, cloudcover_low, cloudcover_total

### Lagged values ###
train_features %>%
  mutate(date = as_date(datetime)) %>%
  filter(is_consumption == 1) %>%
  group_by(date) %>%
  summarise(target = sum(target)) %>%
  mutate(
    target_1 = lag(target, 1),
    target_2 = lag(target, 2),
    target_3 = lag(target, 3),
    target_4 = lag(target, 4),
    target_5 = lag(target, 5),
    target_6 = lag(target, 6),
    target_7 = lag(target, 7),
    target_8 = lag(target, 8),
    target_9 = lag(target, 9)
  ) %>%
  filter(date >= ymd("2021-09-10")) %>%
  select(-date) %>%
  cor() %>%
  corrplot()

# we take the value 7 days prior
