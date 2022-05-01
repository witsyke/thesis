library(tidyverse)
library(ggplot2)

setwd("<<PATH TO THIS DIRECTORY>>")
source("network_util.R")


#------------------------- LOAD DATA ----------------------------------------

# -------- MOVEMENTS
movements <- read_csv("../sir_experiments/data/complete_movement.csv.gz") %>%
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

# -------- DISTANCES
centroids <- read_csv("../data/county_centroids.csv") %>%
  distinct(lk_id, .keep_all = TRUE) %>%
  filter(lk_id != 16056)

county_cross <- centroids %>%
  full_join(centroids, by=character())

distances <- county_cross %>%
  mutate(distance_haversine = haversine_dist(centroid_long.x, centroid_lat.x, centroid_long.y, centroid_lat.y)) %>%
  select(from = lk_id.x, to = lk_id.y, distance_haversine)

# -------- COUNTY DATA
county_data <- read_csv("../data/population_data.csv") %>%
  select(lk_id, lk_movement)

#------------------------- PRE-LOCKDOWN 1 ----------------------------------------

distances_pre <- in_country_movements %>%
  filter(week == "2020-38") %>%
  left_join(county_data, by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data, by = c("kreis2" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to"))


distance_seq_pre <- seq(min(distances_pre$distance_haversine), ceiling(max(distances_pre$distance_haversine)), 1)
distance_proba_pre <- data.frame(type="pre-lockdown", distance = distance_seq_pre, proba = sapply(distance_seq_pre, get_prob_larger_dist, data = distances_pre))

ggplot() +
  geom_line(data = distance_proba_pre, aes(x = distance, y = proba)) +
  scale_y_continuous(trans='log10') +
  xlab("Distance (d) in km") +
  ylab("P(d_ji>= d)")

#------------------------- PARTIAL LOCKDOWN 2 ----------------------------------------
distances_par_lock <- in_country_movements %>%
  filter(week == "2020-47") %>%
  left_join(county_data, by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data, by = c("kreis2" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to"))


distance_seq_par_lock <- seq(min(distances_par_lock$distance_haversine), ceiling(max(distances_par_lock$distance_haversine))+1, 1)
distance_proba_par_lock <- data.frame(type="partial-lockdown", distance = distance_seq_par_lock, proba = sapply(distance_seq_par_lock, get_prob_larger_dist, data = distances_par_lock))

ggplot() +
  geom_line(data = distance_proba_par_lock, aes(x = distance, y = proba)) +
  scale_y_continuous(trans='log10') +
  xlab("Distance (d) in km") +
  ylab("P(d_ji>= d)")


#------------------------- LOCKDOWN 2 ----------------------------------------
distances_lock <- in_country_movements %>%
  filter(week == "2020-53") %>%
  left_join(county_data, by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data, by = c("kreis2" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to"))


distance_seq_lock <- seq(min(distances_lock$distance_haversine), ceiling(max(distances_lock$distance_haversine))+1, 1)
distance_proba_lock <- data.frame(type="lockdown", distance = distance_seq_lock, proba = sapply(distance_seq_lock, get_prob_larger_dist, data = distances_lock))

ggplot() +
  geom_line(data = distance_proba_lock, aes(x = distance, y = proba)) +
  scale_y_continuous(trans='log10') +
  xlab("Distance (d) in km") +
  ylab("P(d_ji>= d)")

#------------------------- PRE VS LOCKDOWN 1 ----------------------------------------
distance_proba_combined <- distance_proba_pre %>%
  dplyr::union(distance_proba_par_lock) %>%
  dplyr::union(distance_proba_lock)


ggplot() +
  geom_line(data = distance_proba_combined, aes(x = distance, y = proba, color = type)) +
  scale_y_continuous(trans='log10') +
  xlab("Distance (d) in km") +
  ylab("P(d_ji>= d)")


