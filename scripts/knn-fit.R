# Note - the actual fitting of the models (starting on line 131) takes a 
# considerable amount of time. It's best to run this on an HPC or at otherwise
# be prepared for roughly 24 hours of run time.

library(tidyverse)
library(here)
library(tidymodels)
library(kknn)

theme_set(theme_minimal(15))
set.seed(8675309)

gaps <- read_csv(here("data", "achievement-gaps-geocoded.csv"))

### Compute weighted average
# lose a couple schools because of missing data
length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

gaps <- gaps %>% 
  group_by(state, year, district_id, school_id, lat, lon, content) %>% 
  summarize(v_hisp = weighted.mean(v_hisp, n, na.rm = TRUE),
            v_econ = weighted.mean(v_econ, n, na.rm = TRUE)) %>%
  ungroup() 

gaps18 <- gaps  %>%
  filter(year == "1718")

gaps <- gaps %>%
  filter(year != "1718")

rec_hisp_all_f <- v_hisp ~ lon + lat + year
rec_hisp_all <- recipe(rec_hisp_all_f, data = gaps) %>%
  step_num2factor(year) %>%
  step_dummy(year) %>%
  step_filter(!is.na(lon), !is.na(lat), !is.na(v_hisp))

rec_hisp_geo_f <- v_hisp ~ lon + lat
rec_hisp_geo <- recipe(rec_hisp_geo_f, data = gaps) %>%
  step_filter(!is.na(lon), !is.na(lat), !is.na(v_hisp))

rec_econ_all_f <- v_econ ~ lon + lat + year
rec_econ_all <- recipe(rec_econ_all_f, data = gaps) %>%
  step_num2factor(year) %>%
  step_dummy(year) %>%
  step_filter(!is.na(lon), !is.na(lat), !is.na(v_econ))

rec_econ_geo_f <- v_econ ~ lon + lat
rec_econ_geo <- recipe(rec_econ_geo_f, data = gaps) %>%
  step_filter(!is.na(lon), !is.na(lat), !is.na(v_econ))

# length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
# length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

# split the data, stratified by district
split <- initial_split(gaps, strata = district_id)

# create training/test data
train <- training(split)
test <- testing(split)

# Create k-fold cv splits
cv_splits <- vfold_cv(train, v = 10, repeats = 5, strata = "district_id")  %>%
  mutate(rec_hisp_all = map(splits, prepper, recipe = rec_hisp_all),
         rec_econ_all = map(splits, prepper, recipe = rec_econ_all),
         rec_hisp_geo = map(splits, prepper, recipe = rec_hisp_geo),
         rec_econ_geo = map(splits, prepper, recipe = rec_econ_geo))


fit_model <- function(rec, spec, form) {
  fit(
    object = spec,
    formula = form,
    data = juice(rec, everything())
    ) 
}

spec_knn_varying <- nearest_neighbor(neighbors = 2) %>%
  set_engine("kknn") %>% 
  set_mode("regression") 


# above pulls out the training data, limits to only the variables in the 
# formula, and removes all missing data

compute_pred <- function(rec, split, model) {
  assess <- assessment(split)
  assess <- bake(rec, new_data = assess)

  pred <- predict(model, new_data = assess) 
  bind_cols(assess, pred)
}

compute_perf <- function(pred_df, gap) {
  
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- metric_set(rmse, rsq)

  numeric_metrics(
    pred_df, 
    truth = !!enquo(gap), 
    estimate = .pred
  )
}

# Optimize number of neighbors for control mod
param_grid <- neighbors %>% 
  range_set(c(1, 50)) %>%
  grid_regular(levels = 50)

spec_knn_varying <- nearest_neighbor(neighbors = varying()) %>%
  set_engine("kknn") %>% 
  set_mode("regression") 

param_grid <- param_grid %>%
  mutate(specs = merge(., spec_knn_varying))


# ###### Examples of above functions
# fit_1 <- fit_model(cv_splits$rec_hisp_geo[[1]], param_grid$specs[[1]], rec_hisp_geo_f)
# compute_pred(cv_splits$rec_hisp_geo[[1]], cv_splits$splits[[1]], fit_1) 
# compute_pred(cv_splits$rec_hisp_geo[[1]], cv_splits$splits[[1]], fit_1) %>%
#   compute_perf(v_hisp)

neighbors <- cv_splits %>%
  mutate(spec_list = list(param_grid)) %>%
  select(splits, id, id2, rec_hisp_all, spec_list) %>%
  unnest(spec_list)

cv <- left_join(cv_splits[1:nrow(cv_splits), ], neighbors[1:nrow(neighbors), ]) 

# fit all models
cv <- cv %>%
  mutate(mod_hisp_all = map2(rec_hisp_all, specs, fit_model, v_hisp ~ .),
         mod_econ_all = map2(rec_econ_all, specs, fit_model, v_econ ~ .),
         mod_hisp_geo = map2(rec_hisp_geo, specs, fit_model, v_hisp ~ .),
         mod_econ_geo = map2(rec_econ_geo, specs, fit_model, v_econ ~ .))
write_rds(cv, here("knn-models", "cv-knn.rds"))
