library(here)
here::i_am("init.R")

source(here::here("R", "libraries.R"))
source(here::here("load_dataset.R"))

# mapping prediction unit id
date_all_prod_units <- train %>%
  filter(is_consumption == 1) %>% #consumers
  group_by(datetime) %>%
  summarise(count = n()) %>%
  filter(count == max(count)) %>%
  pull(datetime) %>%
  max()
ref_prediction_unit_id <- train %>%
  filter(datetime == date_all_prod_units, is_consumption == 1) %>%
  select(prediction_unit_id, county, is_business, product_type)

# IDs with missing train values
id_na_in_train <- train %>%
  group_by(prediction_unit_id, is_consumption) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < max(count)) %>%
  pull(prediction_unit_id) %>%
  unique()