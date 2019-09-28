library(tidyverse)
library(here)
library(tidymodels)
library(kknn)

theme_set(theme_minimal(15))
set.seed(8675309)

gaps <- read_csv(here("data", "achievement-gaps-geocoded-long.csv")) %>% 
  mutate(wave = case_when(year == 1415 ~ 0,
                          year == 1516 ~ 1,
                          year == 1617 ~ 2,
                          year == 1718 ~ 3))

rec_geo <- recipe(v ~ lon + lat, data = gaps)
rec_all <- recipe(v ~ lon + lat + wave, data = gaps)

gaps18 <- gaps %>% 
  group_by(content, gap_group) %>% 
  nest() %>% 
  mutate(data = map(data, ~filter(.x, year == "1718", 
                                  !is.na(lon), 
                                  !is.na(lat), 
                                  !is.na(v))))

gaps <- gaps %>% 
  group_by(content, gap_group) %>% 
  nest() %>% 
  mutate(data = map(data, ~filter(.x, year != "1718", 
                                  !is.na(lon), 
                                  !is.na(lat), 
                                  !is.na(v))))

d <- gaps %>% 
  mutate(data = map(data, ~filter(.x, !is.na(lon), !is.na(lat), !is.na(v))),
         initial_split = map(data, ~initial_split(.x, strata = district_id)),
         train = map(initial_split, training),
         test = map(initial_split, testing))

cv <- d %>% 
  mutate(cv = map(train, ~
                    vfold_cv(.x, v = 10, repeats = 5, strata = "district_id") %>% 
                    mutate(rec_geo = map(splits, prepper, recipe = rec_geo),
                           rec_all = map(splits, prepper, recipe = rec_all))
  )
  ) %>% 
  unnest(cv) %>% 
  gather(model, recipe, starts_with("rec"))

fit_model <- function(rec, spec) {
  fit(
    object = spec,
    formula = v ~ .,
    data = juice(rec, everything())
    ) 
}

# above pulls out the training data, limits to only the variables in the 
# formula, and removes all missing data

compute_pred <- function(rec, split, model) {
  assess <- assessment(split)
  assess <- bake(rec, new_data = assess)

  pred <- predict(model, new_data = assess) 
  bind_cols(assess, pred)
}

compute_perf <- function(pred_df) {
  
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- metric_set(rmse, rsq)

  numeric_metrics(
    pred_df, 
    truth = v, 
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

neighbors <- cv %>%
  mutate(spec_list = list(param_grid)) %>%
  select(content, gap_group, splits, id, id2, model, spec_list) %>%
  unnest(spec_list)

cv <- left_join(cv, neighbors)

# fit all models
cv <- cv %>%
  mutate(models = map2(recipe, specs, fit_model))

# Compute predictions
map3 <- function(x, y, z, fun) {
  pmap(list(x, y, z), ~fun(..1, ..2, ..3))
}

cv <- cv %>% 
  mutate(pred = map3(recipe, splits, models, compute_pred))

# Compute performance
cv <- cv %>% 
  mutate(perf = map(pred, compute_perf))

write_rds(cv, here("knn-models", "cv-knn.rds"))

############## Evaluate model
perf <- cv %>%
  unnest(perf) %>%
  mutate(model = ifelse(grepl("all", model),
                        "Geography + Academic Year", 
                        "Geography Only"))

# summarize by neighbor
perf %>%
  filter(.metric == "rmse") %>%
  group_by(content, model, gap_group, neighbors) %>%
  summarize(rmse = mean(.estimate)) %>%
  ggplot(aes(neighbors, rmse, color = content)) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ gap_group,
              labeller = labeller(gap_group = label_wrap_gen(10))) + 
  scale_color_brewer(palette = "Accent") +
  labs(title = "RMSE on left out folds by neighbor for full model",
       subtitle = "Fit to achievement gap data from 2014-15 to 2016-17") +
  theme(legend.position = "bottom")

ggsave(here("knn-models", "rmse-by-neigh-full.pdf"),
       width = 6.5, height = 8)

# Most frequent best neighbor value
perf %>%
  group_by(id, id2, content, model, gap_group) %>%
  summarize(neighbors = neighbors[which.min(.estimate)],
            rmse      = .estimate[which.min(.estimate)]) %>%
  ggplot(aes(neighbors, fill = model)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Accent") +
  facet_grid(content ~ gap_group) +
  theme(legend.position = "bottom")

best_neigh <- perf %>%
  filter(.metric == "rmse") %>%
  group_by(content, model, gap_group, neighbors) %>%
  summarize(rmse = mean(.estimate)) %>% 
  group_by(model, content, gap_group) %>%
  summarize(neighbors = neighbors[which.min(rmse)],
            rmse      = rmse[which.min(rmse)])

  
best_perf_est <- perf %>%
  semi_join(best_neigh) %>%
  group_by(model, content, gap_group, .metric) %>%
  summarize(mean = mean(.estimate)) %>%
  spread(.metric, mean)

best_perf_var <- perf %>%
  semi_join(best_neigh) %>%
  group_by(model, content, gap_group, .metric) %>%
  summarize(sd = sd(.estimate)) %>%
  spread(.metric, sd) %>%
  rename(rmse_sd = rmse, rsq_sd = rsq)

best_perf <- left_join(best_perf_est, best_perf_var) %>%
  select(model:rmse, rmse_sd, rsq, rsq_sd) %>%
  arrange(gap_group, model)

best_specs <- cv %>% 
  select(neighbors, content, gap_group, specs, recipe, model) %>%
  mutate(model = ifelse(grepl("all", model),
                        "Geography + Academic Year", 
                        "Geography Only")) %>%
  semi_join(best_neigh) %>%
  distinct(neighbors, content, gap_group, model, .keep_all = TRUE) %>%
  filter(model == "Geography Only")

# Test Results
full_train_model <- best_specs %>% 
  select(content, gap_group, recipe, specs) %>% 
  right_join(d) %>% 
  mutate(train_d = map2(recipe, train, ~bake(.x, new_data = .y)),
         fit = map2(specs, train_d, ~fit(.x, v ~ ., .y)),
         test_baked = map2(recipe, test, ~bake(.x, new_data = .y)),
         test_pred = map2(fit, test_baked, ~predict(.x, new_data = .y)),
         test_pred = map2(test_baked, test_pred, bind_cols),
         perf = map(test_pred, compute_perf))

full_train_model %>%
  unnest(perf) %>%
  spread(.metric, .estimate) %>% 
  select(-.estimator) %>% 
  write_csv(here("knn-models", "geo-full-test1517.csv"))

# Evaluate model on 2018 data
test18 <- gaps18 %>% 
  left_join(select(full_train_model, content, gap_group, recipe, fit)) %>% 
  mutate(test18_baked = map2(recipe, data, ~bake(.x, new_data = .y)),
         test18_pred = map2(fit, test18_baked, ~predict(.x, new_data = .y)),
         test18_pred = map2(test18_baked, test18_pred, bind_cols),
         perf = map(test18_pred, compute_perf))

test18 %>%
  unnest(perf) %>%
  spread(.metric, .estimate) %>% 
  select(-.estimator) %>% 
  write_csv(here("knn-models", "geo-full-test18.csv"))
