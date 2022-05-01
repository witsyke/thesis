# TODO similar to what a do for movement change map
# Divide the total in+out movement for each week and county by the total in+out of that county in week 38
library(tidyverse)
setwd("<<PATH TO THIS DIRECTORY>>")
source("extract_movements_per_county.R")


movements <- read_csv("../sir_experiments/data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

# Filter all movements smaller than week 8 as I just assume they have kappa of 1 (similar to week 38)
movements_filtered <- movements %>%
  separate(week, c("year", "week_number"), sep = "-", remove = F) %>%
  mutate(year = as.numeric(year),
         week_number = as.numeric(week_number)) %>%
  filter(year == 2020,
         week >= 38)


baseline_week = "2020-38"
baseline_movements <- extract_total_movement_per_county_single_week(baseline_week, movements_filtered, include_local = TRUE)
weeks <- unique(movements_filtered$week)

time_indicator <- tibble(week = weeks) %>%
  rownames_to_column() %>%
  mutate(time = (as.numeric(rowname) - 1) * 7) %>%
  select(week, time)


get_kappas_for_week <- function(filter_week){
  return(baseline_movements %>%
           left_join(extract_total_movement_per_county_single_week(filter_week, movements_filtered, include_local = TRUE),
                     by = c("kreis"), 
                     suffix = c(".t0", ".t")) %>%
           mutate(week = filter_week, kappa = mean.t / mean.t0) %>%
           select(week, county = kreis, kappa))
}

kappas <- Reduce(rbind, lapply(weeks, get_kappas_for_week)) %>%
  left_join(time_indicator, by = c("week"))
save(kappas, file = "../sir_experiments/data/kappas_baseline_week38.Rdata")
# save(kappas, file = "data/kappas_baseline_week38_all2020.Rdata")
