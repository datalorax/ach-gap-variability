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
  group_by(state, district_id, school_id, lat, lon, content, student_group) %>% 
  summarize(v_hisp = weighted.mean(v_hisp, n, na.rm = TRUE),
            v_econ = weighted.mean(v_econ, n, na.rm = TRUE)) %>%
  ungroup()

length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

# split the data, stratified by district
split <- initial_split(gaps, strata = "district_id")

# create training/test data
train <- training(split)
test <- testing(split)

# Create k-fold cv splits
cv_splits <- vfold_cv(train, v = 10, repeats = 5, strata = "district_id") 

# specify model to fit, start with baseline control model (content and grade)
geo_hisp <- v_hisp ~ lon + lat
geo_econ <- v_econ ~ lon + lat

fit_model <- function(split, spec, form) {
  fit(
    object = spec, 
    formula = form,
    data = na.omit(analysis(split)[all.vars(form)]) 
  )
}

# above pulls out the training data, limits to only the variables in the 
# formula, and removes all missing data

compute_pred <- function(split, model, form) {
  
  # Extract the assessment set
  assess <- na.omit(assessment(split)[all.vars(form)]) 
  
  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)
  
  bind_cols(assess, pred)
}

compute_perf <- function(pred_df) {
  
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- metric_set(rmse, rsq)
  
  numeric_metrics(
    pred_df, 
    truth = v_hisp, 
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
# fit_1 <- fit_model(cv_splits$splits[[1]], param_grid$specs[[1]], geo_control_mod)
# 
# compute_pred(cv_splits$splits[[1]], fit_1)
# 
# compute_pred(cv_splits$splits[[1]], fit_1) %>%
#   compute_perf()

fit_all_specs_one_split <- function(split, spec_df, form) {
  spec_df %>%
    mutate(models = map(specs, 
                         fit_model, 
                         split = split,
                         form = form))
}

# above is same as previous but now takes all specs. it takes the df of specs: param_grid
#fit_all_specs_one_split(cv_splits$splits[[1]], param_grid[1:3, ], geo_only_mod)

fit_all_specs_all_splits <- function(split_df, spec_df, form) {
  split_df %>%
    mutate(models = map(splits, 
                        fit_all_specs_one_split, 
                        form,
                        spec_df = spec_df),
           pred = map2(splits, models, function(split, mod) {
                    map(mod[["models"]], ~compute_pred(split, .x, form))
             }),
           perf = map(pred, ~map_df(.x, compute_perf, .id = "neighbors")),
           perf = map(perf, ~select(., -.estimator) %>% 
                               spread(.metric, .estimate) %>% 
                               mutate(neighbors = as.numeric(neighbors))
                      ),
           )
}

# All fits
fits_geo_hisp <- fit_all_specs_all_splits(cv_splits, param_grid, geo_hisp)
fits_geo_econ <- fit_all_specs_all_splits(cv_splits, param_grid, geo_econ)

dir.create("knn-models")
write_rds(fits_geo_hisp, here("knn_models", "geo-knn-hisp.rds"))
write_rds(fits_geo_econ, here("knn_models", "geo-knn-econ.rds"))


mods <- list(cntrl, geo_cntrl, geo)

### Best K
unnest_grid <- function(mod) {
  mod %>%
    select(id, id2, perf) %>% 
    unnest()
}

rmse_by_neighbors <- function(mod) {
  mod %>%
    unnest_grid() %>%
    group_by(neighbors) %>%
    summarize(rmse = mean(rmse)) 
}

best_neighbor <- function(mod) {
  mod %>%
    unnest_grid() %>%
    group_by(id, id2) %>%
    summarize(neighbors = neighbors[which.min(rmse)],
              rmse      = rmse[which.min(rmse)])
}

pull_min_neigh <- function(mod) {
  mod %>%
    unnest_grid() %>%
    filter(rmse == min(rmse)) %>%
    pull(neighbors)
}

best_spec <- function(k) {
  param_grid %>%
    filter(neighbors == k) %>%
    pull(specs) %>%
    .[[1]]
}

mods2 <- tibble(mdl = mods,
               rmse_neigh = map(mdl, rmse_by_neighbors),
               best_neigh_by = map(mdl, best_neighbor),
               best_neigh_val = map_dbl(mdl, pull_min_neigh),
               best_spec = map(best_neigh_val, best_spec),
               plot_neigh_rmse = map2(rmse_neigh, mdl,
                                     ~ggplot(.x, aes(neighbors, rmse)) +
                                        geom_line(aes(color = id, group = id),
                                                  unite(unnest_grid(.y),
                                                        id,
                                                        id,
                                                        id2),
                                                  alpha = 0.5) +
                                        geom_point() +
                                        geom_line() +
                                        guides(color = FALSE) +
                                        scale_color_viridis_d()))

mods2$plot_neigh_rmse

spec_knn <- nearest_neighbor(neighbors = 2) %>%
  set_engine("kknn")

cv_splits <- cv_splits %>%
  mutate(models_knn = map(splits, fit_model, spec_knn, geo_only_mod),
         pred_knn = map2(splits, models_knn, compute_pred),
         perf_knn = map(pred_knn, compute_perf))

cv_splits %>%
  unnest(perf_knn) %>%
  group_by(.metric) %>%
  summarise(.estimate = mean(.estimate))

cv_splits %>%
  unnest(perf_knn) %>%
  group_by(.metric) %>%
  summarise(
    .estimate_mean = mean(.estimate),
    .estimate_sd = sd(.estimate)
  )
cv_splits %>% 
  unnest(perf_knn) %>% 
  filter(.metric == "rsq") %>% 
  arrange(.estimate)

holdout_results <- 
  cv_splits %>%
  unnest(pred_knn) %>%
  mutate(.resid = v_hisp - .pred)

ggplot(holdout_results, aes(.pred, v_hisp)) +
  geom_point()





perf <- map(mods2$mdl, ~pull(.x, "perf")) 
map2(perf, mods2$best_neigh_val, ~filter(.x, neighbors == .y))  

perf[[1]]

mods2 %>% 
  mutate(perf_best = map(mdl, ~map(.x, ~pull(.x, "perf"))))

%>% 
  mutate(best_mod = map(geo_cv$perf, 
                           ~filter(.x, neighbors == mods2$best_neigh_val))) %>% 
  select(id, id2, best_mod) %>% 
  unnest() %>% 
  summarize(rsq = mean(rsq))


#
# #### ---- Not run
# cntrl$spec_perf[[1]]
# cv_splits <- cv_splits %>%
#   mutate(models_knn = map(splits, fit_model, spec_knn, geo_only_mod),
#          pred_knn = map2(splits, models_knn, compute_pred),
#          perf_knn = map(pred_knn, compute_perf))
#
# cv_splits %>%
#   unnest(perf_knn) %>%
#   group_by(.metric) %>%
#   summarise(.estimate = mean(.estimate))
#
# cv_splits %>%
#   unnest(perf_knn) %>%
#   group_by(.metric) %>%
#   summarise(
#     .estimate_mean = mean(.estimate),
#     .estimate_sd = sd(.estimate)
#   )
# cv_splits %>%
#   unnest(perf_knn) %>%
#   filter(.metric == "rsq") %>%
#   arrange(.estimate)
#
#
# # The control model explains almost nothing, adding geography ups R^2 to about
# # 10%, but the model is actually overfit. If we remove the control variables
# # and just model geographical variance, we account for approximately 28% of the
# # variance. Note that we can see the overfitting because of the cross-validation
# # procedure. Standard approaches of evaluating the model results against the
# # data used to fit the model would result in an overly optimistic representation,
# # as R^2 would also increase.
#
#
#
#
# holdout_results <-
#   cv_splits %>%
#   unnest(pred_knn) %>%
#   mutate(.resid = v_hisp - .pred)
#
