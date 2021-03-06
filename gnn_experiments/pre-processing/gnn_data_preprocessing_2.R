library(tidyverse) 

start_date = "2020-02-24"
end_date = "2020-12-31"

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
  ungroup() %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(name = lk_movement, date, AnzahlFall) %>%
  filter(date >= as.Date(start_date),
         date <= as.Date(end_date),
         complete.cases(.)) %>%
  pivot_wider(name, names_from = date, values_from = AnzahlFall) %>%
  arrange(name) %>%
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname) - 1)

active_cases_temp %>%
  filter(!complete.cases(.))



write_csv(active_cases_temp, "<<OUTPUT PATH VERTEX LABELS>>")
# TODO save this

# ---------------- LOCKDOWN INDICATOR BOOLEAN ------------------------------

lockdown_bool <- active_cases_full %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(name = lk_movement, date) %>%
  mutate(lockdown = if_else((date >= as.Date("2020-03-13") & date <= as.Date("2020-05-04")) | (date >= as.Date("2020-11-04") & date <= as.Date("2021-01-31")), TRUE, FALSE)) %>%
  filter(date >= as.Date(start_date),
         date <= as.Date(end_date),
         complete.cases(.)) %>%
  pivot_wider(name, names_from = date, values_from = lockdown, values_fn = {mean}) %>%
  arrange(name) %>%
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname) - 1)

write_csv(lockdown_bool, "<<OUTPUT PATH BINARY LOCKDOWN INDICATOR>>")




#-----------------------ADJACENCY LIST------------------------------------------------------------

setwd("<<PATH TO MOVEMENT DATA DIRECTORY>>")

# Load movement data and aggregate Berlin
movement_data <- read_csv("complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()



edge_list_week <- movement_data %>%
  filter(complete.cases(.)) %>%
  select(week, kreis1, kreis2, travellers) %>%
  mutate(travellers = round(travellers / 7, digits = 2),
         travellers_binary = if_else(travellers > 0, 1, 0)) 



date = seq(as.Date(start_date), as.Date(end_date), by="day")
dates = tibble(date)%>%
  rownames_to_column()

lockdown_movement <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-")) %>%
  mutate(match = if_else(year == 2021 & week == 53, "2020-53", match)) %>%
  select(date, week = match, idx = rowname) %>%
  left_join(kappas, by = c("week" = "week")) %>%
  filter(complete.cases(.)) %>%
  select(date, name = county, kappa) %>%
  mutate(kappa = round(kappa, digits = 2)) %>%
  pivot_wider(name, names_from = date, values_from = kappa) %>%
  arrange(name) %>%
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname) - 1)


write_csv(lockdown_movement, "<<OUTPUT PATH FOR MOVEMENT CHANGE INDICATOR>>")



covid_txs_edgelist_timed <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-")) %>%
  mutate(match = if_else(year == 2021 & week == 53, "2020-53", match)) %>%
  select(date, week = match, idx = rowname) %>%
  left_join(edge_list_week, by = c("week" = "week")) %>%
  filter(complete.cases(.)) %>%
  select(idx, date, kreis1, kreis2, travellers, travellers_binary)


view(covid_txs_edgelist_timed %>%
  filter(date == as.Date("2020-04-25")))
  

for (i in 1:length(dates$rowname)){
  print(i)
  
  write_csv(covid_txs_edgelist_timed %>%
              filter(idx == i) %>%
              select(kreis1, kreis2, travellers),
            paste("OUTPUT PATH FOR EDGE WEIGHTS NORMALIZED", "GER_",
                  date[i],
                  ".csv",
                  sep = ""),
            col_names = FALSE)
  
  write_csv(covid_txs_edgelist_timed %>%
              filter(idx == i) %>%
              select(kreis1, kreis2, travellers), 
            paste("OUTPUT PATH FOR EDGE WEIGHTS UNNORMALIZED", "GER_",
                  date[i],
                  ".csv", 
                  sep = ""),
            col_names = FALSE) 
} 
