library(tidyverse)
library(ggplot2)

#------------------------- LOAD DATA ----------------------------------------
setwd("<<PATH TO THIS DIRECTORY>>")

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
  filter(travellers_day > 0) # can probably threshold here as too small movements wouldn't be visible

in_country_movements_filtered <- in_country_movements %>%
  filter(kreis1 != kreis2) # self-loops don't need to be plotted

centroids <- read_csv("../data/county_centroids.csv") %>%
  distinct(lk_id, .keep_all = TRUE) %>%
  filter(lk_id != 16056)


county_data <- read_csv("../data/population_data.csv")

# ------------------- PRE-LOCKDOWN ----------------------------

movements_pre <- in_country_movements_filtered %>%
  filter(week == "2020-38")

edges_pre <- movements_pre %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis2" = "lk_movement")) %>%
  select(from = kreis1, to = kreis2, travellers_day, from_id = lk_id.x, to_id = lk_id.y) %>%
  left_join(centroids, by = c("from_id" = "lk_id")) %>%
  left_join(centroids, by = c("to_id" = "lk_id")) %>%
  select(from_long = centroid_long.x, from_lat = centroid_lat.x, to_long = centroid_long.y, to_lat = centroid_lat.y, travellers_day)

(plot_pre <- ggplot() +
  geom_segment(data = edges_pre, aes(x = from_lat, y = from_long, xend = to_lat, yend = to_long, size = travellers_day, alpha = travellers_day)) +
  geom_point(data = centroids, aes(x = centroid_lat, y = centroid_long), size = 15) +
  scale_size_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.6, 15)) +
  scale_alpha_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.0022, 2)) +
  coord_map() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(size = 24),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()))


# ------------------- PARTIAL LOCKDOWN ----------------------------
movements_par_lock <- in_country_movements_filtered %>%
  filter(week == "2020-47")

edges_par_lock <- movements_par_lock %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis2" = "lk_movement")) %>%
  select(from = kreis1, to = kreis2, travellers_day, from_id = lk_id.x, to_id = lk_id.y) %>%
  left_join(centroids, by = c("from_id" = "lk_id")) %>%
  left_join(centroids, by = c("to_id" = "lk_id")) %>%
  select(from_long = centroid_long.x, from_lat = centroid_lat.x, to_long = centroid_long.y, to_lat = centroid_lat.y, travellers_day)

(plot_par <- ggplot() +
  geom_segment(data = edges_par_lock, aes(x = from_lat, y = from_long, xend = to_lat, yend = to_long, size = travellers_day, alpha = travellers_day)) +
  geom_point(data = centroids, aes(x = centroid_lat, y = centroid_long), size = 15) +
  scale_size_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.6, 15)) +
  scale_alpha_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.0022, 2)) +
  coord_map() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(size = 24),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()))


# ------------------- LOCKDOWN ----------------------------
movements_lock <- in_country_movements_filtered %>%
  filter(week == "2020-53")

edges_lock <- movements_lock %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis1" = "lk_movement")) %>%
  left_join(county_data %>% select(lk_id, lk_movement), by = c("kreis2" = "lk_movement")) %>%
  select(from = kreis1, to = kreis2, travellers_day, from_id = lk_id.x, to_id = lk_id.y) %>%
  left_join(centroids, by = c("from_id" = "lk_id")) %>%
  left_join(centroids, by = c("to_id" = "lk_id")) %>%
  select(from_long = centroid_long.x, from_lat = centroid_lat.x, to_long = centroid_long.y, to_lat = centroid_lat.y, travellers_day)

(plot_full <- ggplot() +
  geom_segment(data = edges_lock, aes(x = from_lat, y = from_long, xend = to_lat, yend = to_long, size = travellers_day, alpha = travellers_day)) +
  geom_point(data = centroids, aes(x = centroid_lat, y = centroid_long), size = 15) +
  scale_size_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.6, 15)) +
  scale_alpha_continuous(limits = c(min(edges_pre$travellers_day), max(edges_pre$travellers_day)), range = c(0.0022, 2)) +
  coord_map() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(size = 24),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()))
