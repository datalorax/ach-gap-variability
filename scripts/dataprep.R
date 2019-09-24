library(tidyverse)
library(stringr)
library(fs)
library(rio)
library(pracma)
library(here)
library(janitor)

wa_files <- dir_ls(here("data", "wa_2018"))
or_files <- dir_ls(here("data", "or_2018"))
ca_files <- dir_ls(here("data", "ca_2018"))

### Oregon Prep
or <- map_df(or_files, import, setclass = "tbl_df", 
             .id = "file", 
             na = c("-", "--", "*")) %>%
  clean_names() %>%
  mutate(content = gsub(".+_(ela|math)_.+", "\\1", file),
         content = ifelse(content == "ela", "ELA", "Math"),
         year = gsub(".+_(\\d\\d\\d\\d)\\.xlsx", "\\1", file)) %>%
  select(district, district_id, school, school_id, content, year, 
         student_group, grade_level, 
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
  arrange(year, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(year, district_id, school_id, content, grade, student_group) %>%
  add_count(wt = number) %>%
  ungroup() %>%
  mutate(percent = (number/n)*100) %>%
  select(-number)

### Washington Prep
wa <- map_df(wa_files, import, setclass = "tbl_df", 
             .id = "file", 
             na = c("-", "--", "*")) %>%
  clean_names() %>%
  mutate(year = gsub(".+_(\\d\\d\\d\\d-\\d\\d)_.+", "\\1", file),
         year = gsub("20|-", "", year)) %>%
  filter(test_administration_group == "General") %>%
  rename(district = district_name,
         district_id = district_code,
         school = school_name,
         school_id = school_code,
         grade = grade_level,
         content = test_subject,
         n = count_of_students_expected_to_test_including_previously_passed) %>%
  select(district, district_id, school, school_id, content, year, 
         student_group, grade, n, 
         percent_level1, percent_level2, percent_level3, percent_level4) %>%
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
  arrange(year, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  drop_na(percent, school_id) %>%
  filter(!(grepl("Total", school) | grepl("Total", district)))

### California Prep
ca <- map_df(ca_files[grep("all", ca_files)], read_csv,
             .id = "file", 
             na = c("-", "--", "*"),
             col_types = cols(.default = "c")) %>%
  clean_names() %>%
  filter(school_code != "0000000") %>% 
  mutate(year = gsub(".+ca(\\d\\d\\d\\d).+", "\\1", file),
         year = gsub("20", "", year),
         year = paste0(as.numeric(year) - 1, year),
         content = ifelse(test_id == 1, "ELA", "Math")) %>%
  rename(district_id = district_code,
         school_id = school_code,
         n = students_with_scores,
         p1 = percentage_standard_not_met,
         p2 = percentage_standard_nearly_met,
         p3 = percentage_standard_met,
         p4 = percentage_standard_exceeded) %>%
  select(district_id, school_id, content, year, subgroup_id, grade, 
         n, p1, p2, p3, p4)

ca <- ca %>%
  gather(performance_level, percent, matches("^p\\d")) %>%
  mutate(performance_level = parse_number(performance_level), 
         percent = ifelse(percent == "*", NA, percent),
         percent = parse_number(percent),
         grade = parse_number(grade),
         n = parse_number(n)) %>%
  filter(as.numeric(school_id) > 0) %>%
  drop_na(percent)

ca_entities <- map_df(ca_files[grep("entities", ca_files)], read_csv,
             .id = "file", 
             na = c("-", "--", "*"),
             col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(year = gsub(".+ca(\\d\\d\\d\\d).+", "\\1", file),
         year = gsub("20", "", year),
         year = paste0(as.numeric(year) - 1, year)) %>%
  rename(district_id = district_code,
         school_id = school_code,
         district = district_name, 
         school = school_name) %>%
  select(district_id, district, school_id, school, year) %>%
  filter(as.numeric(school_id) > 0)

ca <- left_join(ca, ca_entities)

ca_subgroups <- map_df(ca_files[grep("Subgroups", ca_files)], read_csv,
             .id = "file", 
             na = c("-", "--", "*"),
             col_types = cols(.default = "c"),
             col_names = FALSE) %>%
  clean_names()  %>%
  setNames(c("file", "code_string", "subgroup_id", 
             "student_group", "variable")) %>%
  select(subgroup_id, student_group, variable)

ca <- left_join(ca, ca_subgroups) %>%
  filter(variable == "Ethnicity"| 
           variable == "Economic Status" |
           variable == "All Students") %>%
  select(district, district_id, school, school_id, content, year,
         student_group, grade, performance_level, n, percent) %>%
  filter(!is.na(percent),
         student_group == "Hispanic or Latino"|
           student_group == "White" |
           student_group == "Economically disadvantaged" |
           student_group == "All Students") %>%
  mutate(student_group = 
           case_when(student_group == "Hispanic or Latino" ~ "Hispanic",
                     student_group == "Economically disadvantaged" ~ "Econ Dis",
                     student_group == "All Students" ~ "Total Pop",
                     TRUE ~ student_group)) %>%
  mutate_at(vars(district_id, school_id), list(as.double)) %>% 
  filter(n != 0)


### Bind together
d <- bind_rows(ca, or, wa, .id = "state") %>%
  mutate(state = case_when(state == 1 ~ "CA",
                           state == 2 ~ "OR",
                           state == 3 ~ "WA"),
         prop = percent/100) %>%
  select(-percent) %>% 
  filter(n > 5)


# Prep for auc
eth <- d %>%
  filter(student_group == "Hispanic" | student_group == "White") %>%
  arrange(state, year, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(state, year, district_id, school_id, content, grade, student_group) %>%
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
  distinct(state, year, district_id, school_id, content, grade, student_group) %>%
  group_by(state, year, district_id, school_id, content, grade) %>%
  count() %>%
  filter(n == 2) %>%
  select(-n) %>%
  ungroup() 

econ_dis <- semi_join(d, econ_school_select) %>%
  filter(student_group == "Econ Dis" | student_group == "Total Pop") %>%
  arrange(state, year, district_id, school_id, content, grade, student_group,
          performance_level) %>%
  group_by(state, year, district_id, school_id, content, grade, student_group) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  select(-prop, -n) %>%
  spread(performance_level, cum_prop) %>%
  mutate(`0` = 0) %>%
  gather(performance_level, cum_prop, `1`:`0`, convert = TRUE) %>%
  spread(student_group, cum_prop) %>%
  arrange(state, year, district_id, school_id, content, grade, performance_level) %>%
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
  group_by(state, year, district_id, school_id, content, grade) %>%
  summarize(v_hisp = v(Hispanic, White)) %>%
  drop_na(v_hisp, district_id) %>%
  ungroup()

econ_gaps <- econ_dis %>%
  group_by(state, year, district_id, school_id, content, grade) %>%
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
ccd_files <- dir_ls(here("data", "geographic"))

ccd15 <- read_delim(ccd_files[grep("\\.txt", ccd_files)], delim = "\t") %>%
  clean_names() %>%
  filter(stabr %in% c("CA", "OR", "WA")) %>%
  mutate(year = gsub("20|-", "", survyear)) %>%
  select(school_id = st_schid, state = stabr, year, ncessch)


ccd16 <- read_csv(ccd_files[grep("1516", ccd_files)]) %>%
  clean_names() %>%
  filter(stabr %in% c("CA", "OR", "WA")) %>%
  mutate(year = gsub("20|-", "", survyear)) %>%
  select(school_id = st_schid, state = stabr, year, ncessch) %>%
  separate(school_id, c(NA, "school_id"), sep = "-")

ccd17 <- read_csv(ccd_files[grep("1617_", ccd_files)]) %>%
  clean_names() %>%
  filter(st %in% c("CA", "OR", "WA")) %>%
  mutate(year = gsub("20|-", "", school_year)) %>%
  select(school_id = st_schid, state = st, year, ncessch) %>%
  separate(school_id, c(NA, NA, "school_id"), sep = "-")

ccd18 <- read_csv(ccd_files[grep("1718_", ccd_files)]) %>%
  clean_names() %>%
  filter(st %in% c("CA", "OR", "WA")) %>%
  mutate(year = gsub("20|-", "", school_year)) %>%
  select(school_id = st_schid, state = st, year, ncessch) %>%
  separate(school_id, c(NA, NA, "school_id"), sep = "-")

ccd <- bind_rows(ccd15, ccd16, ccd17, ccd18)

ccd %>%
  count(state, nchar(school_id))

gaps <- gaps %>%
  mutate(school_id = case_when(state == "CA" ~ 
                                 str_pad(school_id, 7, "left", pad = "0"),
                               state == "OR" ~ 
                                 str_pad(school_id, 20, "left", pad = "0"),
                               state == "WA" ~ 
                                 str_pad(school_id, 4, "left", pad = "0"))
  )

gaps <- left_join(gaps, ccd) 
table(is.na(gaps$ncessch))

edge <- import(here("data", "geographic",
                         "EDGE_GEOCODE_PUBLICSCH_1617.xlsx"),
                    setclass = "tbl_df") %>% 
  clean_names() %>%
  select(state, ncessch, lat, lon, city, cnty, nmcnty, cbsatype, cbsa, nmcbsa) %>% 
  mutate(cbsatype = case_when(cbsatype == 1 ~ "Metropolitan",
                              cbsatype == 2 ~ "Micropolitan", 
                              TRUE ~ "Non-CBSA"))

gaps <- left_join(gaps, edge) %>%
  select(year, state, district_id, school_id, ncessch, 
         content, grade, lat, lon,city, cnty, nmcnty, cbsatype, cbsa, nmcbsa, 
         v_hisp, v_econ)

gaps %>%
  count(year, missing = is.na(lat)) %>%
  group_by(year) %>%
  mutate(percent = n/sum(n)) %>%
  select(-n) %>%
  spread(missing, percent)

# Add N's back in
ns <- d %>%
  select(-performance_level, -prop, -district, -school) %>%
  filter(student_group == "Total Pop") %>% 
  distinct(.keep_all = TRUE) %>% 
  select(-student_group) %>%
  mutate(school_id = case_when(state == "CA" ~ 
                                 str_pad(school_id, 7, "left", pad = "0"),
                               state == "OR" ~ 
                                 str_pad(school_id, 20, "left", pad = "0"),
                               state == "WA" ~ 
                                 str_pad(school_id, 4, "left", pad = "0"))
  )

gaps <- left_join(gaps, ns)

# Add cohort variable
cohorts <- expand.grid(year = c("1415", "1516", "1617", "1718"), 
                       grade = c(3:8, 11, 13)) %>%
  arrange(grade, year) %>%
  mutate(cohort = c(6:3, 7:4, 8:5, 9:6, 10:7, 11:8, 12:9, 13:10),
         cohort = ifelse(year == "1718", grade, cohort),
         cohort = case_when(grade == 11 & year == "1617" ~ 12,
                            grade == 11 & year == "1516" ~ 13,
                            grade == 11 & year == "1415" ~ 14,
                            grade == 13 & year == "1617" ~ 14,
                            grade == 13 & year == "1516" ~ 15,
                            grade == 13 & year == "1415" ~ 16, 
                            TRUE ~ as.double(cohort)),
         year = as.character(year)) 

gaps <- left_join(gaps, cohorts)

# fill in missing lon lat if the school has it in any year for any grade
gaps <- gaps %>%
  group_by(school_id) %>%
  fill(lat)

gaps <- gaps %>%
  group_by(school_id) %>%
  fill(lon)

length(unique(gaps$school_id[!is.na(gaps$v_hisp)]))
length(unique(gaps$school_id[!is.na(gaps$v_econ)]))

# write out csv
gaps %>% 
  arrange(state, year, district_id, school_id, content, grade) %>% 
write_csv(here("data", "achievement-gaps-geocoded.csv"))

# write out feather file
gaps %>% 
  arrange(state, year, district_id, school_id, content, grade) %>% 
feather::write_feather(here("data", "achievement-gaps-geocoded.feather"))
