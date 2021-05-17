# Steve Pittard
# Example todymodel code

library(tidymodels)
library(recipes)
library(parsnip)
library(workflows)

url <- "https://raw.githubusercontent.com/steviep42/bios534_spring_2020/master/data/pima.csv"
pm <- read.csv(url)  # The Pima Indians Data Set

# Split Data into Train and Test data sets
data_split <- initial_split(pm, prop = .80)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Setup a Random Forest Model
rf_mod <- rand_forest(trees = 10) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Create a workflow which uses this Model
pm_workflow <-  workflow() %>% add_model(rf_mod)

# Now produce a fitted training model using the above info
random_rf_mod <- pm_workflow %>%
  add_formula(diabetes ~ .) %>% fit(data = train_data)

# Get some probabilities with which to make a ROC Curve
simple_rf_probs <-
  predict(random_rf_mod, test_data, type = "prob") %>% 
  bind_cols(test_data)

# Plot a ROC Curve
simple_glm_roc <- simple_rf_probs %>% roc_curve(diabetes, .pred_pos) 
autoplot(simple_glm_roc)


