# TODO should be able to use this for the scaled plots as well, if I want to make them
library(tidyverse)

#------------------------- LOAD DATA ----------------------------------------

setwd("<<PATH TO SIR EXPERIEMENT>>") # same data is reused
# -------- MOVEMENTS
movements <- read_csv("data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()


# EXTRACT GERMAN MOVEMENTS
in_country_movements <- movements %>%
  filter(complete.cases(.)) %>%
  mutate(travellers_day = travellers / 7) %>%
  filter(travellers_day >= 0) # later i would need the week again to iterate


#------------------------- TOTAL AMOUNT OF TRAVEL BY WEEK ----------------------------------------
total_weekly_movement <- in_country_movements %>%
  group_by(week) %>%
  summarise(sum_travellers_day = sum(travellers_day)) 

#------------------------- SELECT BASELINE WEEK ----------------------------------------
t0 <- "2020-38"
#------------------------- EQUAL AMOUNT OF TRAVEL ----------------------------------------
# week T * sum(week t0)/sum(week T)

scaling_factors_total_travel <- total_weekly_movement %>%
  mutate(sum_travellers_week_t0 = (total_weekly_movement %>% filter(week == t0))$sum_travellers_day,
         scaling_factor = sum_travellers_week_t0 / sum_travellers_day) %>%
  select(week, scaling_factor)


scaled_movements_total_travel <- in_country_movements %>%
  left_join(scaling_factors_total_travel, by = c("week")) %>%
  select(week, from = kreis1, to = kreis2, travellers_day, scaling_factor) %>%
  mutate(scaled_travellers_day = travellers_day * scaling_factor) %>%
  select(week, from, to, scaled_travellers_day)

save(scaled_movements_total_travel, file = "PATH TO TOTAL TRAVEL SCALED DATA")
# TODO need to preprocess this to produce the gnn data <- could use preprocessing script for normal data I guess


#------------------------- EQUAL STRUCTURE ----------------------------------------
# same as in Schlosser et al. 
# week t0 * sum(week T)/sum(week t0) 

scaling_factors_structure <- total_weekly_movement %>%
  mutate(sum_travellers_week_t0 = (total_weekly_movement %>% filter(week == t0))$sum_travellers_day,
         scaling_factor = sum_travellers_day / sum_travellers_week_t0) %>%
  select(week, scaling_factor)

scaled_movements_structure <- in_country_movements %>%
  filter(week == t0) %>%
  full_join(scaling_factors_structure, by = character()) %>%
  select(week = week.y, from = kreis1, to = kreis2, travellers_day, scaling_factor) %>%
  mutate(scaled_travellers_day = travellers_day * scaling_factor) %>%
  select(week, from, to, scaled_travellers_day) 

save(scaled_movements_structure, file = "PATH TO STRUCTURE SCALED DATA")
