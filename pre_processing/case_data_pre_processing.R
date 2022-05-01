library(tidyverse)
library(readr)
setwd("<<PATH TO THIS DIRECTORY>>")

case_data <- read_csv("<<PATH TO MOST RECENT CASE DATA>>")
population_data <- read_csv("../data/population_data.csv")

case_data <- case_data %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006), 11000))

# Summarise new cases by LK and Ref date
data_by_lk_date <- case_data %>%
  group_by(IdLandkreis, Refdatum) %>%
  summarise(new_cases = sum(AnzahlFall))


 

# Add population and Bundesland
data_by_lk_date <- data_by_lk_date %>%
  left_join(population_data %>% select(-bundesland), by = c("IdLandkreis" = "lk_id"))

# Normalize new_cases by population
norm_data_by_lk_date <- data_by_lk_date %>%
  mutate(norm_cases = (new_cases / population) * 100000)
# 

# Generate date sequence
date = seq(as.Date("2020/1/1"), as.Date("2021/10/25"), by="day")
dates = tibble(date)
  

# Generate time sequence for each LK (in order to generate 0s for dates with no new cases)
lk_dates <- population_data %>%
  select(lk_id, bundesland) %>%
  full_join(dates, by=character())

# Join the summarized case data with the date sequence and set cases for dates with no cases to 0
cases_by_lk_date <- lk_dates %>%
  left_join(norm_data_by_lk_date, by=c("lk_id" = "IdLandkreis", "date" = "Refdatum")) %>%
  select(lk_id, date, norm_cases, bundesland) %>%
  mutate(norm_cases = replace(norm_cases, is.na(norm_cases), 0)) # maybe replace_na works here as well

# Add week and Summarise by week
cases_by_lk_week <- cases_by_lk_date %>%
  mutate(week = as.double(strftime(date, format = "%V")),
         year = as.double(strftime(date, format = "%Y"))) %>%
  mutate(year = ifelse(week == 53 & year == 2021, year-1, year)) %>%
  group_by(bundesland, lk_id, year, week) %>%
  summarise(mean_new_cases = mean(norm_cases))
  
# Number is very different form 7-Incidence as there is an additional divide by 7

# Change format to lk x date
cases_per_lk_by_week <- cases_by_lk_week %>%
  mutate(year_week = paste(year , week, sep = "-")) %>%
  ungroup() %>%
  select(-year, -week) %>%
  pivot_wider(names_from = year_week, values_from = mean_new_cases)

write_csv(cases_per_lk_by_week, "<<OUTPUT PATH>>")

# Change format to date x lk
cases_per_week_by_lk <- cases_by_lk_week %>%
  ungroup() %>%
  select(-bundesland) %>%
  pivot_wider(names_from = lk_id, values_from = mean_new_cases)

write_csv(cases_per_week_by_lk, "<<OUTPUT PATH>>")

# when exactly do i have to use ungroup

t <- case_data %>% 
  filter(NeuerFall == 1, NeuerTodesfall != 1, NeuGenesen != 1)

sum(t$AnzahlFall)

