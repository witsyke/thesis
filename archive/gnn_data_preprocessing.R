library(tidyverse)
library(readr)

start_date = "2020-03-01"
end_date = "2021-10-10"

# INTERNATIONAL ACTIVE CASES
setwd("<<PATH TO CASE DATA DIRECTORY>>")
#-----------------------NODE FEATURES-------------------------------------------------------------

# Load national population data
county_data <- read_csv("<<PATH TO POPULATION DATA>>")

active_cases <- read_csv("<<PATH TO ESTIMATED CASE DATA>>")

# Load international population data
population_data <- read_csv("<<PATH TO INTERNATIONAL POPULATION DATA>>") %>%
  select(country = `Country Name`, country_code = `Country Code`, population = `2020`) %>%
  mutate(population = ifelse(country_code == "ERI", 3213969, population))

# Generate date sequence
date = seq(as.Date(start_date)-7, as.Date(end_date), by="day")
dates = tibble(date)


# Generate time sequence for each LK (in order to generate 0s for dates with no new cases)
lk_dates <- active_cases %>%
  select(IdLandkreis) %>%
  distinct(IdLandkreis) %>%
  full_join(dates, by=character())



active_cases_full <- lk_dates %>%
  left_join(active_cases) %>%
  mutate(AnzahlFall = ifelse(is.na(AnzahlFall), 0, AnzahlFall))


active_cases_temp <- active_cases_full %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup() 

active_cases_temp_final <- active_cases_temp %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 1), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 2), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 3), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 4), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 5), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 6), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  left_join(active_cases_temp %>%
              mutate(date = date + 7), by = c("IdLandkreis" = "IdLandkreis", "date" = "date")) %>%
  rename(active_cases_t = AnzahlFall.x, 
         active_cases_t_1 = AnzahlFall.y, 
         active_cases_t_2 = AnzahlFall.x.x, 
         active_cases_t_3 = AnzahlFall.y.y, 
         active_cases_t_4 = AnzahlFall.x.x.x, 
         active_cases_t_5 = AnzahlFall.y.y.y, 
         active_cases_t_6 = AnzahlFall.x.x.x.x, 
         active_cases_t_7 = AnzahlFall.y.y.y.y) %>%
  filter(date >= as.Date(start_date))
# CAREFUL: these t_1 means t-1
  
active_cases_final <- active_cases_temp_final %>%  
  filter(date >= as.Date(start_date)) %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  filter(complete.cases(.)) %>%
  mutate(lk_movement = paste("a", lk_movement, sep = "_")) %>%
  select(node_id = lk_movement, 
         date,
         active_cases_t, 
         active_cases_t_1, 
         active_cases_t_2,
         active_cases_t_3,  
         active_cases_t_4,  
         active_cases_t_5, 
         active_cases_t_6, 
         active_cases_t_7,
         population) 


# LOAD MOVEMENT
movement_data_temp <- read_csv("<<PATH TO INTERNATIONAL MOVEMENTS>>") %>%
  pivot_longer(-start, names_to = "dest", values_to = "prob") %>%
  select(dest) %>%
  unique(.)


# Read case, recovery and death data
cases <- read_csv("<<PATH TO INTERNATIONAL CASE DATA>>")

# Basic data cleaning function
aggregate_countries <- function(data_frame) {
  return(data_frame %>%
           filter(`Country/Region` != "#country+name") %>%
           group_by(`ISO 3166-1 Alpha 3-Codes`) %>%
           summarise_if(is.numeric, sum))
}

# Basic data cleaning
cases_by_country <- aggregate_countries(cases)

# Function to enlongate data frames
enlongate <- function(data_frame, id, key, value) {
  return(data_frame %>%
           pivot_longer(-id, names_to = key, values_to = value))
}
# Elongate data frames
cases_long <- enlongate(cases_by_country, "ISO 3166-1 Alpha 3-Codes", "date", "cases")

cases_long_shifted <- cases_long %>%
  mutate(date = as.Date(date, "%m/%d/%y"),
         date_shifted = date - 15) # Subtracting 15 days to get the recovery date [t, t+14]

int_active_cases <- cases_long_shifted  %>%
  left_join(cases_long_shifted %>% select(-date_shifted), suffix = c(".orig", ".shifted"),
            by = c("ISO 3166-1 Alpha 3-Codes" = "ISO 3166-1 Alpha 3-Codes", "date_shifted" = "date"),
            keep = TRUE) %>%
  mutate(cases.shifted = replace(cases.shifted, is.na(cases.shifted), 0),
         active_cases = cases.orig - cases.shifted) %>%
  select(country_code = `ISO 3166-1 Alpha 3-Codes.orig`, date = date.orig, active_cases, cases.orig, cases.shifted) %>%
  select(country_code, date, active_cases) %>%
  mutate(active_cases = ifelse(active_cases < 0, 0, active_cases))

int_active_cases_temp <- int_active_cases %>%
  left_join(int_active_cases %>%
              mutate(date = date + 1), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 2), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 3), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 4), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 5), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 6), by = c("country_code" = "country_code", "date" = "date")) %>%
  left_join(int_active_cases %>%
              mutate(date = date + 7), by = c("country_code" = "country_code", "date" = "date")) %>%
  rename(active_cases_t = active_cases.x, 
         active_cases_t_1 = active_cases.y, 
         active_cases_t_2 = active_cases.x.x, 
         active_cases_t_3 = active_cases.y.y, 
         active_cases_t_4 = active_cases.x.x.x, 
         active_cases_t_5 = active_cases.y.y.y, 
         active_cases_t_6 = active_cases.x.x.x.x, 
         active_cases_t_7 = active_cases.y.y.y.y) %>%
  filter(date >= as.Date(start_date),
         date <= as.Date(end_date))
# CAREFUL: these t_1 means t-1

int_active_cases_temp_final <- int_active_cases_temp %>%
  left_join(population_data, by = c("country_code" = "country_code")) %>%
  select(country, 
         date,
         active_cases_t, 
         active_cases_t_1, 
         active_cases_t_2,
         active_cases_t_3,  
         active_cases_t_4,  
         active_cases_t_5, 
         active_cases_t_6, 
         active_cases_t_7,
         population) 

# Use movement to filter extract countries that actually have movement data
int_active_cases_final <- movement_data_temp %>%
  left_join(int_active_cases_temp_final, by = c("dest" = "country")) %>%
  rename(node_id = dest) %>%
  filter(complete.cases(.)) %>%
  mutate(node_id = paste("b", node_id, sep = "_"))

# Find data without covid data
no_case_data <- movement_data_temp %>%
  left_join(int_active_cases_temp_final, by = c("dest" = "country")) %>%
  filter(!complete.cases(.)) %>%
  select(country = dest) %>%
  mutate(country = paste("b", country, sep = "_"))

all_active_cases <- active_cases_final %>%
  union(int_active_cases_final)



times <- all_active_cases %>%
  select(date) %>%
  distinct(date) %>%
  rownames_to_column() %>%
  mutate(timestep = as.double(rowname) - 1) %>%
  select(original_time = date, contiguos_time = timestep)

write_csv(times, "<<OUTPUT PATH>>")

node_idx <- all_active_cases %>%
  select(node_id) %>%
  distinct(node_id) %>%
  arrange(node_id) %>%
  rownames_to_column() %>%
  mutate(node_id_new = as.double(rowname) - 1) %>%
  select(originalId = node_id, contiguosId = node_id_new)

write_csv(node_idx, "<<OUTPUT PATH>>")


node_features_time <- all_active_cases %>%
  left_join(times, by = c("date" = "original_time")) %>%
  left_join(node_idx, by = c("node_id" = "originalId")) %>%
  arrange(contiguos_time, contiguosId) %>%
  mutate(contiguosId = sprintf(contiguosId, fmt = '%#.1f'),
         contiguos_time = sprintf(contiguos_time, fmt = '%#.1f')) %>%
  select(contiguosId, 
         contiguos_time,
         active_cases_t, 
         active_cases_t_1, 
         active_cases_t_2,
         active_cases_t_3,  
         active_cases_t_4,  
         active_cases_t_5, 
         active_cases_t_6, 
         active_cases_t_7,
         population)

write_csv(node_features_time, "<<OUTPUT PATH>>", col_names = F)


#-----------------------ADJACENCY LIST------------------------------------------------------------

setwd("<<PATH TO MOVEMENT DATA>>")

# Load movement data and aggregate Berlin
movement_data <- read_csv("complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()


# TODO very unsure about the normalization of the edges - if i divide by total travel out of a county wouldn't i loose the information
# that more people move from one county to another even if they are them same in terms of relative movement 
# Better to use flux fraction? travels/ total travels
# TODO would it make sense to scale the data again by county population? -> this would mean I don't lose 
# scaling information (one county is bigger and more people move -> 0.1 from A->B != 0.1 from B->C)
# TODO test it out
# TODO i dont have the devices for foreign countries --> no normalization possible there
# what happens if i dont normalize --> test it out
# current normalization does not make sense as balance between counties is destroyed
# ASK in bernhard meeting
# might be able to avoid this by only using counties for now
edge_list_week <- movement_data %>%
  mutate(kreis1 = if_else(!is.na(kreis1), paste("a", kreis1, sep = "_"), kreis1),
         kreis2 = if_else(!is.na(kreis2), paste("a", kreis2, sep = "_"), kreis2),
         staat1 = if_else(is.na(kreis1), paste("b", staat1, sep = "_"), staat1),
         staat2 = if_else(is.na(kreis2), paste("b", staat2, sep = "_"), staat2)) %>%
  mutate(kreis1 = if_else(is.na(kreis1), staat1, kreis1),
         kreis2 = if_else(is.na(kreis2), staat2, kreis2)) %>%
  select(week, start = kreis1, destination = kreis2, travellers) %>%
  filter(start != "b_Germany",
         destination != "Germany") %>%
  filter(!start %in% no_case_data$country,
         !destination %in% no_case_data$country) %>%
  # group_by(week, start) %>%
  # mutate(sum_outward = sum(travellers)) %>%
  # ungroup() %>%
  # mutate(weight = travellers / sum_outward) %>%
  left_join(node_idx, by = c("start" = "originalId")) %>%
  left_join(node_idx, by = c("destination" = "originalId")) %>%
  select(txId1 = contiguosId.x, txId2 = contiguosId.y, weight = travellers, week)


date = seq(as.Date(start_date), as.Date(end_date), by="day")
dates = tibble(date)

covid_txs_edgelist_timed <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-")) %>%
  mutate(match = if_else(year == 2021 & week == 53, "2020-53", match)) %>%
  select(date, week = match) %>%
  left_join(times, by = c("date" = "original_time")) %>%
  arrange(contiguos_time) %>%
  left_join(edge_list_week, by = c("week" = "week")) %>%
  filter(complete.cases(.)) %>%
  select(txId1, txId2, timestep = contiguos_time, weight)
  
write_csv(covid_txs_edgelist_timed, "<<OUTPUT PATH>>")  

