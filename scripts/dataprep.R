library(tidyverse)
library(fs)
library(rio)
library(pracma)
library(here)
library(janitor)
theme_set(theme_minimal(15))

wa_files <- dir_ls(here("data", "wa_2018"))
or_files <- dir_ls(here("data", "or_2018"))
ca_files <- dir_ls(here("data", "ca_2018"))

### Oregon Prep
or <- map_df(or_files, import, setclass = "tbl_df", # nrow(or) = 55748
             .id = "file", 
             na = c("-", "--", "*")) %>%
  clean_names() %>%
  mutate(content = gsub(".+_(ela|math)_.+", "\\1", file),
         content = ifelse(content == "ela", "ELA", "Math")) %>%
  select(district, district_id, school, school_id, content, student_group,
         grade_level, 
         number_level_1, number_level_2, number_level_3, number_level_4)

or <- or %>% 
  gather(performance_level, number, starts_with("number")) %>% 
  rename(grade = grade_level) %>%
  filter(!is.na(number),
         student_group == "Hispanic/Latino" |
           student_group == "White" |
           student_group == "Econo. Disadvantaged"|
           student_group == "Total Population (All Students)") %>%
  mutate(grade = parse_number(grade),
         performance_level = parse_number(performance_level),
         student_group = 
           case_when(
            student_group == "Hispanic/Latino" ~ "Hispanic",
            student_group == "Econo. Disadvantaged" ~ "Econ Dis",
            student_group == "Total Population (All Students)" ~ "Total Pop",
            TRUE ~ student_group))

or <- or %>%
  arrange(district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(district_id, school_id, content, grade, student_group) %>%
  add_count(wt = number) %>%
  ungroup() %>%
  mutate(percent = (number/n)*100) %>%
  select(-number)

### Washington Prep
wa <- import(wa_files, setclass = "tbl_df") %>% 
  clean_names() %>%
  filter(test_administration_group == "General") %>%
  rename(district = district_name,
         district_id = district_code,
         school = school_name,
         school_id = school_code,
         grade = grade_level,
         content = test_subject,
         n = count_of_students_expected_to_test_including_previously_passed) %>%
  select(district, district_id, school, school_id, content, student_group,
         grade, n, percent_level1, percent_level2, percent_level3, 
         percent_level4) %>%
  filter(content != "Science")

wa <- wa %>%  
  gather(performance_level, percent, starts_with("percent")) %>%
  filter(student_group == "Hispanic/ Latino of any race(s)" |
           student_group == "White" |
           student_group == "Low-Income" |
           student_group == "All Students",
         grade != "All Grades") %>%
  mutate(grade = parse_number(grade), 
         performance_level = parse_number(performance_level),
         percent = parse_number(percent),
         student_group = 
           case_when(
            student_group == "Hispanic/ Latino of any race(s)" ~ "Hispanic",
            student_group == "Low-Income" ~ "Econ Dis",
            student_group == "All Students" ~ "Total Pop",
            TRUE ~ student_group),
         content = ifelse(content == "English Language Arts", 
                          "ELA", 
                          content)) %>%
  arrange(district_id, school_id, content, grade, student_group,
          performance_level) %>%
  drop_na(percent, school_id) %>%
  filter(!(grepl("Total", school) | grepl("Total", district)))

### California Prep
ca <- import(ca_files[grep("all", ca_files)], 
             setclass = "tbl_df") %>%
  clean_names() %>%
  mutate(content = ifelse(test_id == 1, "ELA", "Math")) %>%
  rename(district_id = district_code,
         school_id = school_code,
         n = students_with_scores,
         p1 = percentage_standard_not_met,
         p2 = percentage_standard_nearly_met,
         p3 = percentage_standard_met,
         p4 = percentage_standard_exceeded) %>%
  select(district_id, school_id, content, subgroup_id, grade, 
         n, p1, p2, p3, p4)

ca <- ca %>%
  gather(performance_level, percent, matches("^p\\d")) %>%
  mutate(performance_level = parse_number(performance_level), 
         percent = ifelse(percent == "*", NA, percent),
         percent = parse_number(percent)) %>%
  filter(school_id != 0)

ca_entities <- import(ca_files[grep("entities", ca_files)], 
                      setclass = "tbl_df") %>%
  clean_names() %>%
  rename(district_id = district_code,
         school_id = school_code,
         district = district_name, 
         school = school_name) %>%
  select(district_id, district, school_id, school) %>%
  filter(school_id != 0)

ca <- left_join(ca, ca_entities)

ca_subgroups <- import(ca_files[grep("Subgroups", ca_files)], 
                       setclass = "tbl_df") %>%
  setNames(c("code_string", "subgroup_id", "student_group", "variable")) %>%
  select(subgroup_id, student_group, variable)

ca <- left_join(ca, ca_subgroups) %>%
  filter(variable == "Ethnicity"| 
           variable == "Economic Status" |
           variable == "All Students") %>%
  select(district, district_id, school, school_id, content, student_group,
         grade, performance_level, n, percent) %>%
  filter(!is.na(percent),
         student_group == "Hispanic or Latino"|
           student_group == "White" |
           student_group == "Economically disadvantaged" |
           student_group == "All Students") %>%
  mutate(student_group = 
           case_when(student_group == "Hispanic or Latino" ~ "Hispanic",
                     student_group == "Economically disadvantaged" ~ "Econ Dis",
                     student_group == "All Students" ~ "Total Pop",
                     TRUE ~ student_group),
           n = as.numeric(n)) 

### Bind together
d <- bind_rows(ca, or, wa, .id = "state") %>%
  mutate(state = case_when(state == 1 ~ "CA",
                           state == 2 ~ "OR",
                           state == 3 ~ "WA"),
         prop = percent/100) %>%
  select(-percent)

# Prep for auc
eth <- d %>%
  filter(student_group == "Hispanic" | student_group == "White") %>%
  arrange(state, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(state, district_id, school_id, content, grade, student_group) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  select(-prop, -n) %>%
  spread(performance_level, cum_prop) %>% 
  mutate(`0` = 0) %>%
  gather(performance_level, cum_prop, `1`:`0`, convert = TRUE) %>%
  spread(student_group, cum_prop) %>%
  arrange(state, district_id, school_id, content, grade, performance_level) %>%
  ungroup()

econ_school_select <- d %>% # Restrict to schools that have econ dis & all students reported
  filter(student_group == "Econ Dis" | student_group == "Total Pop") %>%
  distinct(state, district_id, school_id, content, grade, student_group) %>%
  group_by(state, district_id, school_id, content, grade) %>%
  count() %>%
  filter(n == 2) %>%
  select(-n) %>%
  ungroup() 

econ_dis <- semi_join(d, econ_school_select) %>%
  filter(student_group == "Econ Dis" | student_group == "Total Pop") %>%
  arrange(state, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(state, district_id, school_id, content, grade, student_group) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  select(-prop, -n) %>%
  spread(performance_level, cum_prop) %>%
  mutate(`0` = 0) %>%
  gather(performance_level, cum_prop, `1`:`0`, convert = TRUE) %>%
  spread(student_group, cum_prop) %>%
  arrange(state, district_id, school_id, content, grade, performance_level) %>%
  ungroup()

# Calculate effect sizes	
v <- function(x, y) {
  if(any(is.na(c(x, y)))) {
    return(NA_real_)
  }
  auc <- pracma::trapz(x, y)	
  sqrt(2)*qnorm(auc)
}

hisp_gaps <- eth %>%
  group_by(state, district_id, school_id, content, grade) %>%
  summarize(v_hisp = v(Hispanic, White)) %>%
  drop_na(v_hisp, district_id) %>%
  ungroup()

econ_gaps <- econ_dis %>%
  group_by(state, district_id, school_id, content, grade) %>%
  summarize(v_econ = v(`Econ Dis`, `Total Pop`)) %>%
  drop_na(v_econ, district_id) %>%
  ungroup()

length(unique(hisp_gaps$school_id)) / length(unique(d$school_id))
length(unique(econ_gaps$school_id)) / length(unique(d$school_id))

length(unique(hisp_gaps$school_id));length(unique(econ_gaps$school_id))
gaps <- full_join(hisp_gaps, econ_gaps) 
length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

### Add in geocoded data
ccd <- read_csv(here("data", "geographic", 
                     "ccd_sch_029_1718_w_0a_03302018.csv")) %>%
  clean_names() %>% 
  rename(school_id = schid)

ccd <- ccd %>% 
  filter(st %in% c("CA", "OR", "WA")) %>% 
  select(sch_name, st_schid, ncessch) %>% 
  separate(st_schid, c("state", "district_id", "school_id"), sep = "-") %>% 
  mutate(school_id = parse_number(school_id)) %>% 
  select(-district_id)

gaps <- left_join(gaps, ccd)

edge <- import(here("data", "geographic",
                         "EDGE_GEOCODE_PUBLICSCH_1617.xlsx"),
                    setclass = "tbl_df") %>% 
  clean_names()

gaps <- left_join(gaps, edge, by = c("state", "ncessch")) %>%
  select(state, district_id, school_id, sch_name, ncessch, 
         city, cnty, nmcnty, lat, lon, content, grade, v_hisp, v_econ)


# Add N's back in
ns <- d %>%
  select(-performance_level, -prop) %>%
  group_by(state, district_id, school_id, content, student_group) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup()

gaps <- left_join(gaps, ns)

length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

gaps %>% 
  select(-district, -school, -sch_name) %>% 
  select(state:grade, n, v_hisp, v_econ) %>% 
  arrange(state, district_id, school_id, content, grade) %>% 
write_csv(here("data", "achievement-gaps-geocoded.csv"))
