#################### Decompose variance in achievement gaps ####################
library(tidyverse)
library(here)
library(lme4)

gaps <- read_csv(here("data", "achievement-gaps-geocoded.csv"))

gaps_model_d <- gaps %>%
	mutate(state = relevel(factor(state), ref = "CA"),
		     content = relevel(factor(content), ref = "ELA"))

base_var_hisp <- lmer(v_hisp ~ content + grade +
                   (1|school_id) + (1|district_id) + (1|cnty) + (1|state), 
                 gaps_model_d)
summary(base_var_hisp)

base_var_econ <- lmer(v_econ ~ content + grade +
                   (1|school_id) + (1|district_id) + (1|cnty) + (1|state), 
                 gaps_model_d)
summary(base_var_econ)


extract_var <- function(mod) {
	as.data.frame(VarCorr(mod)) %>%
		select(component = grp, variance = vcov) %>%
		mutate(proportion = variance / sum(variance))	
}
vars <- map_df(list("Hispanic/White" = base_var_hisp, 
										"Economic Disadvantage/All Students" = base_var_econ), 
	     				 extract_var, 
	     				 .id = "model")

lbls <- c("Schools", "Districts", "Counties", "States", "Residual")

better_labels <- tibble(label = factor(lbls, levels = lbls),
	                      component = unique(vars$component)) 

vars <- left_join(vars, better_labels)

label_locs <- vars %>%
	filter(model == "Hispanic/White") %>%
	arrange(desc(label)) %>%
	mutate(cumsum = cumsum(proportion),
			   cumsum_lag = lag(cumsum),
			   cumsum_lag = ifelse(is.na(cumsum_lag), 0, cumsum_lag),
			   midpoint = (cumsum + cumsum_lag)/2) 

ggplot(vars, aes(model, proportion)) +
	geom_col(aes(fill = label),
		       alpha = 0.8) +
	geom_text(aes(label = label, y = midpoint), label_locs, 
						size = 5,
						color = "gray20",
						nudge_x = 0.5,
						hjust = "left") +
	scale_x_discrete("Achievement Gap", expand = c(0, 0)) +
	expand_limits(x = 2.8) +
	guides(fill = "none") +
	labs(x = "Achievement Gap",
		   y = "Proportion of Total Variability") +
	colorblindr::scale_fill_OkabeIto() +
	theme_minimal(15) #+
	theme(plot.margin = margin(5, 5, 5, 5, "cm"))

