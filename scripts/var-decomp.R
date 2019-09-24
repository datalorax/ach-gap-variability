#################### Decompose variance in achievement gaps ####################
library(tidyverse)
library(here)
library(lme4)

gaps <- read_csv(here("data", "achievement-gaps-geocoded.csv"))

gaps_model_d <- gaps %>%
	mutate(state = relevel(factor(state), ref = "CA"),
		     content = relevel(factor(content), ref = "ELA"))
  
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

base_var_econ <- lmer(v_econ ~ content + grade +
                        (1|school_id) + (1|district_id) + (1|cnty) + 
                        (1|year) + (1|cohort) + (1|state),  
                 gaps_model_d)

all_fits_econ <- allFit(base_var_econ)
summary(all_fits_econ)
# nmkbw is okay, all others are not. Again, estimates largely look identical, 
# although Nelder mead is a bit different

base_var_econ <- update(base_var_econ, 
                        control = lmerControl(optimizer = "nmkbw"))

summary(base_var_econ)

extract_var <- function(mod) {
	as.data.frame(VarCorr(mod)) %>%
		select(component = grp, variance = vcov) %>%
		mutate(proportion = variance / sum(variance))	
}
vars_raw <- map_df(list("Hispanic/White" = base_var_hisp, 
										"Economic Disadvantage/All Students" = base_var_econ), 
	     				 extract_var, 
	     				 .id = "model")

lbls <- c("Schools", "Districts", "Counties", "Psuedo-Cohorts", 
          "Academic Year", "States", "Residual")

better_labels <- tibble(label = factor(lbls, levels = lbls),
	                      component = unique(vars_raw$component)) 

vars <- left_join(vars_raw, better_labels) %>% 
  filter(label != "Psuedo-Cohorts",
         label != "Academic Year")

label_locs <- vars %>%
	filter(model == "Hispanic/White",
	       label != "Psuedo-Cohorts",
	       label != "Academic Year") %>%
	arrange(desc(label)) %>%
	mutate(cumsum = cumsum(proportion),
			   cumsum_lag = lag(cumsum),
			   cumsum_lag = ifelse(is.na(cumsum_lag), 0, cumsum_lag),
			   midpoint = (cumsum + cumsum_lag)/2) 

perc_label_locs <- vars %>%
	group_by(model) %>%
	arrange(desc(label)) %>%
	mutate(lag = ifelse(is.na(lag(proportion)), 0, lag(proportion)),
		     cumsum = cumsum(lag))
vars_raw %>% 
  filter(component == "cohort" |
         component == "year") %>% 
  group_by(model) %>% 
  summarize(total_prop = sum(proportion)*100)

ggplot(vars, aes(model, proportion)) +
	geom_col(aes(fill = label),
		       alpha = 0.8) +
	geom_text(aes(label = label, y = midpoint), label_locs, 
						size = 5,
						color = "gray20",
						nudge_x = 0.5,
						hjust = "left") +
	geom_text(aes(label = paste0(round(proportion, 2)*100, "%"), y = cumsum),
		        perc_label_locs,
		        nudge_y = 0.02,
		        color = "gray30") +
	scale_x_discrete("Achievement Gap", expand = c(0, 0)) +
	expand_limits(x = 2.8) +
	scale_y_continuous(labels = scales::percent) +
	guides(fill = "none") +
	labs(x = "Achievement Gap",
		   y = "Percentage of Total Variability",
		   caption = "Variance associated with pseudo-cohorts and years omitted, which combined accounted for approximately 0.05% of the total variance in each model") +
	colorblindr::scale_fill_OkabeIto() +
	theme_minimal(15) 

