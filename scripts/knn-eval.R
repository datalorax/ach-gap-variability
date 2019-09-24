library(tidyverse)
library(here)
library(tidymodels)
library(kknn)

theme_set(theme_minimal(15))

cv <- read_rds(here("knn-models", "cv-knn.rds"))

# compute predictions on the left out cases
map3 <- function(x, y, z, fun) {
  pmap(list(x, y, z), ~fun(..1, ..2, ..3))
}

cv <- cv %>%
  mutate(mod_hisp_all_pred = map3(
           rec_hisp_all, splits, mod_hisp_all, compute_pred),
         mod_econ_all_pred = map3(
           rec_econ_all, splits, mod_econ_all, compute_pred),
         mod_hisp_geo_pred = map3(
           rec_hisp_geo, splits,mod_hisp_geo, compute_pred),
         mod_econ_geo_pred = map3(
           rec_econ_geo, splits, mod_econ_geo, compute_pred))
write_rds(cv, here("knn-models", "cv-knn.rds"))

# compute performance
cv <- cv %>%
  mutate(mod_hisp_all_perf = map(mod_hisp_all_pred, compute_perf, v_hisp),
         mod_econ_all_perf = map(mod_econ_all_pred, compute_perf, v_econ),
         mod_hisp_geo_perf = map(mod_hisp_geo_pred, compute_perf, v_hisp),
         mod_econ_geo_perf = map(mod_econ_geo_pred, compute_perf, v_econ))
write_rds(cv, here("knn-models", "cv-knn.rds"))

perf <- cv %>%
  select(id, id2, neighbors, ends_with("perf")) %>%
  gather(model, performance, ends_with("perf")) %>%
  unnest() %>%
  separate(model, c(NA, "outcome", "model", NA)) %>%
  mutate(outcome = ifelse(outcome == "econ", 
                          "Economically Disadvantaged/All Students", 
                          "Hispanic/White"),
         model = ifelse(model == "all", 
                        "Geography + Academic Year", 
                        "Geography Only"))

# summarize by neighbor
perf %>%
  filter(.metric == "rmse") %>%
  group_by(model, outcome, neighbors) %>%
  summarize(rmse = mean(.estimate)) %>%
  ggplot(aes(neighbors, rmse, color = model)) +
    geom_line() +
    geom_point() +
    facet_wrap(~outcome) + 
    scale_color_brewer(palette = "Accent") +
    theme(legend.position = "bottom")

# Most frequent best neighbor value
perf %>%
  group_by(id, id2, model, outcome) %>%
  summarize(neighbors = neighbors[which.min(.estimate)],
              rmse      = .estimate[which.min(.estimate)]) %>%
  ggplot(aes(neighbors, fill = model)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Accent") +
    facet_wrap(~outcome) +
    theme(legend.position = "bottom")

best_neigh <- perf %>%
  filter(.metric == "rmse") %>%
  group_by(model, outcome) %>%
  nest() %>%
  mutate(neighbors = map_dbl(data, ~.x[which.min(.x$.estimate), 
                                      "neighbors", 
                                      drop = TRUE]),
         rmse     = map_dbl(data, ~.x[which.min(.x$.estimate), 
                                      ".estimate", 
                                      drop = TRUE])) %>%
  select(-data)


best_perf_est <- perf %>%
  semi_join(best_neigh) %>%
  group_by(model, outcome, .metric) %>%
  summarize(mean = mean(.estimate)) %>%
  spread(.metric, mean)

best_perf_var <- perf %>%
  semi_join(best_neigh) %>%
  group_by(model, outcome, .metric) %>%
  summarize(sd = sd(.estimate)) %>%
  spread(.metric, sd) %>%
  rename(rmse_sd = rmse, rsq_sd = rsq)

best_perf <- left_join(best_perf_est, best_perf_var) %>%
  select(model:rmse, rmse_sd, rsq, rsq_sd) %>%
  arrange(outcome, model)

best_specs <- cv %>% 
  select(neighbors, specs, starts_with("rec")) %>%
  gather(model, recipe, starts_with("rec")) %>%
  separate(model, c(NA, "outcome", "model")) %>%
  mutate(outcome = ifelse(outcome == "econ", 
                          "Economically Disadvantaged/All Students", 
                          "Hispanic/White"),
         model = ifelse(model == "all", 
                        "Geography + Academic Year", 
                        "Geography Only")) %>%
  semi_join(best_neigh) %>%
  distinct(neighbors, outcome, model, .keep_all = TRUE) %>%
  filter(model == "Geography Only")


# Test Results
full_train_model <- best_specs %>%
  mutate(out_var = c(v_hisp ~ ., v_econ ~ .),
         train_d = map(recipe, bake, new_data = train),
         fit = map3(specs, out_var, train_d, fit),
         test_baked = map(recipe, bake, new_data = test),
         test_pred = map2(fit, test_baked, ~predict(.x, new_data = .y)),
         test_pred = map2(test_baked, test_pred, bind_cols),
         perf = map2(test_pred, list("v_hisp", "v_econ"), compute_perf))

full_train_model %>%
  select(outcome, model, perf) %>%
  unnest() %>%
  spread(.metric, .estimate)

# Evaluate model on 2018 data
test18 <- best_specs %>%
  mutate(out_var = c(v_hisp ~ ., v_econ ~ .),
         train_d = map(recipe, bake, new_data = train),
         fit = map3(specs, out_var, train_d, fit),
         test18_baked = map(recipe, bake, new_data = gaps18),
         test18_pred = map2(fit, test18_baked, ~predict(.x, new_data = .y)),
         test18_pred = map2(test18_baked, test18_pred, bind_cols),
         perf = map2(test18_pred, list("v_hisp", "v_econ"), compute_perf))

test18 %>%
  select(outcome, model, perf) %>%
  unnest() %>%
  spread(.metric, .estimate)
