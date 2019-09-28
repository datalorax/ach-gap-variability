#################### Decompose variance in achievement gaps ####################
library(tidyverse)
library(here)
library(lme4)

gaps <- read_csv(here("data", "achievement-gaps-geocoded.csv"))

# Remove grade 10 - too few cases
gaps <- gaps %>% 
  filter(grade != 10)

gaps_model_d <- gaps %>%
  mutate(state = relevel(factor(state), ref = "CA"),
         content = relevel(factor(content), ref = "ELA"),
         grade = relevel(factor(grade), ref = "3"))

base_var_hisp <- lmer(v_hisp ~ content + grade +
                        (1|school_id) + (1|district_id) + (1|cnty) + 
                        (1|year) + (1|cohort) + (1|state), 
                      gaps_model_d)
# failed to converge

all_fits_hisp <- allFit(base_var_hisp)
summary(all_fits_hisp)

summary(base_var_hisp)
# Nelder_Mead, nlminbwrap, and nmkbw all converge fine. Models look 
# functionally similar though

base_var_hisp <- update(base_var_hisp, 
                        control = lmerControl(optimizer = "nmkbw"))
summary(base_var_hisp)

base_var_black <- lmer(v_black ~ content + grade +
                         (1|school_id) + (1|district_id) + (1|cnty) + 
                         (1|year) + (1|cohort) + (1|state),  
                       gaps_model_d, 
                       control = lmerControl(optimizer = "nmkbw"))
summary(base_var_black)

base_var_econ <- lmer(v_econ ~ content + grade +
                        (1|school_id) + (1|district_id) + (1|cnty) + 
                        (1|year) + (1|cohort) + (1|state),  
                      gaps_model_d, 
                      control = lmerControl(optimizer = "nmkbw"))
summary(base_var_econ)

# Fit simplified models without cohort
base_var_hisp <- update(base_var_hisp, . ~ . - (1|cohort))
summary(base_var_hisp)

base_var_black <- update(base_var_black, . ~ . - (1|cohort))
summary(base_var_black)

base_var_econ <- update(base_var_econ, . ~ . - (1|cohort))
summary(base_var_econ)


# Fit simplified models without year
base_var_hisp <- update(base_var_hisp, . ~ . - (1|year))
summary(base_var_hisp)

base_var_black <- update(base_var_black, . ~ . - (1|year))
summary(base_var_black)

base_var_econ <- update(base_var_econ, . ~ . - (1|year))
summary(base_var_econ)

dir.create(here("manuscript", "var-decomp-models"))
write_rds(base_var_hisp, here("manuscript", "var-decomp-models", "hisp-white.rds"))
write_rds(base_var_black, here("manuscript", "var-decomp-models", "black-white.rds"))
write_rds(base_var_econ, here("manuscript", "var-decomp-models", "econ-all.rds"))