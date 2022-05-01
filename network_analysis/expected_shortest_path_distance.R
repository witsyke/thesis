library(tidyverse)
library(ggplot2)

setwd("<<PATH TO THIS DIRECTORY>>")
source("network_util.R")
#------------------------- LOAD DATA ----------------------------------------

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
county_data <- read_csv("PATH TO COUNTY DATA") %>%
  select(lk_id, lk_movement)


# ------------------------ PRE-LOCKDOWN -----------------------------------
mobility_graph_pre <- construct_graph("2020-38")
shortest_paths_mobility_pre <- get_shortest_paths(mobility_graph_pre)
average_shortest_path_t0 <- calculate_mean_shortest_path(shortest_paths_mobility_pre)

shortest_paths_df_pre <- data.frame(shortest_paths_mobility_pre)
colnames(shortest_paths_df_pre) <- rownames(shortest_paths_df_pre)

distance_shortest_path_pre <- shortest_paths_df_pre %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  rename(from_name = rowname, to_name = name, shortest_path_length = value) %>%
  left_join(county_data, by = c("from_name" = "lk_movement")) %>%
  left_join(county_data, by = c("to_name" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to")) %>%
  select(from = from_name, to = to_name, shortest_path_length, distance_haversine)

# TODO need to count how many edges are at distance +- epsilon 


dist_seq_pre <- seq(0, round(max(distance_shortest_path_pre$distance_haversine)), 1)


expected_path_length_distance_pre <- data.frame(distance = dist_seq_pre, 
                                               expected_path_length = sapply(dist_seq_pre, 
                                                              get_relative_shortest_path_length_by_distance,
                                                              baseline_mean_spl = average_shortest_path_t0,
                                                              epsilon = 1,
                                                              data = distance_shortest_path_pre)) %>%
  mutate(type = "Pre-lockdown",
         dash = "a")

ggplot() +
  geom_line(data = expected_path_length_distance_pre, aes(x = distance, y  = expected_path_length, group = 1)) 


# ------------------------ PARTIAL LOCKDOWN -----------------------------------
mobility_graph_par_lock <- construct_graph("2020-47")
shortest_paths_mobility_par_lock <- get_shortest_paths(mobility_graph_par_lock)

shortest_paths_df_par_lock <- data.frame(shortest_paths_mobility_par_lock)
colnames(shortest_paths_df_par_lock) <- rownames(shortest_paths_df_par_lock)

distance_shortest_path_par_lock <- shortest_paths_df_par_lock %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  rename(from_name = rowname, to_name = name, shortest_path_length = value) %>%
  left_join(county_data, by = c("from_name" = "lk_movement")) %>%
  left_join(county_data, by = c("to_name" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to")) %>%
  select(from = from_name, to = to_name, shortest_path_length, distance_haversine)

# TODO need to count how many edges are at distance +- epsilon 


dist_seq_par_lock <- seq(0, round(max(distance_shortest_path_par_lock$distance_haversine)), 1)


expected_path_length_distance_par_lock <- data.frame(distance = dist_seq_par_lock, 
                                                 expected_path_length = sapply(dist_seq_par_lock, 
                                                                               get_relative_shortest_path_length_by_distance,
                                                                               baseline_mean_spl = average_shortest_path_t0,
                                                                               epsilon = 1,
                                                                               data = distance_shortest_path_par_lock)) %>%
  mutate(type = "Partial-lockdown",
         dash = "a")
# TODO should I smooth this? is this what they are doing?
ggplot() +
  geom_line(data = expected_path_length_distance_par_lock, aes(x = distance, y  = expected_path_length, group = 1))


# ------------------------ LOCKDOWN -----------------------------------
mobility_graph_lock <- construct_graph("2020-53")
shortest_paths_mobility_lock <- get_shortest_paths(mobility_graph_lock)

shortest_paths_df_lock <- data.frame(shortest_paths_mobility_lock)
colnames(shortest_paths_df_lock) <- rownames(shortest_paths_df_lock)

distance_shortest_path_lock <- shortest_paths_df_lock %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  rename(from_name = rowname, to_name = name, shortest_path_length = value) %>%
  left_join(county_data, by = c("from_name" = "lk_movement")) %>%
  left_join(county_data, by = c("to_name" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to")) %>%
  select(from = from_name, to = to_name, shortest_path_length, distance_haversine)



dist_seq_lock <- seq(0, round(max(distance_shortest_path_lock$distance_haversine)), 1)


expected_path_length_distance_lock <- data.frame(distance = dist_seq_lock, 
                                               expected_path_length = sapply(dist_seq_lock, 
                                                                             get_relative_shortest_path_length_by_distance,
                                                                             baseline_mean_spl = average_shortest_path_t0,
                                                                             epsilon = 1,
                                                                             data = distance_shortest_path_lock)) %>%
  mutate(type = "Lockdown",
         dash = "a")

ggplot() +
  geom_line(data = expected_path_length_distance_lock, aes(x = distance, y  = expected_path_length, group = 1))

# ------------------------ COMBINED -----------------------------------
expected_path_length_distance_combined <- expected_path_length_distance_pre %>%
  dplyr::union(expected_path_length_distance_par_lock) %>%
  dplyr::union(expected_path_length_distance_lock)

ggplot() +
  geom_line(data = expected_path_length_distance_combined, aes(x = distance, y  = expected_path_length, group = type, color = type), alpha = 0.4, size = 1.2) + 
  geom_smooth(data = expected_path_length_distance_combined, aes(x = distance, y  = expected_path_length, group = type, color = type), size = 2) +
  ylab("Expected shortest path length \n (Relative to L of week 38)") +
  xlab("Distance d (km)") +
  theme_bw() +
  theme(legend.position = c(0.16, 0.86),
        axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.text = element_text(size=30),
        legend.key.size = unit(3,"line"))

# ------------------------------------------------------------
# ------------------------ PARTIAL LOCKDOWN SCALED-----------------------------------
read(file = "week_47_scaled.Rdata")
mobility_graph_par_lock_scaled <- construct_graph_from_df(week_47_scaled)
shortest_paths_mobility_par_lock_scaled <- get_shortest_paths(mobility_graph_par_lock_scaled)

shortest_paths_df_par_lock_scaled <- data.frame(shortest_paths_mobility_par_lock_scaled)
colnames(shortest_paths_df_par_lock_scaled) <- rownames(shortest_paths_df_par_lock_scaled)

distance_shortest_path_par_lock_scaled <- shortest_paths_df_par_lock_scaled %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  rename(from_name = rowname, to_name = name, shortest_path_length = value) %>%
  left_join(county_data, by = c("from_name" = "lk_movement")) %>%
  left_join(county_data, by = c("to_name" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to")) %>%
  select(from = from_name, to = to_name, shortest_path_length, distance_haversine)



dist_seq_par_lock_scaled <- seq(0, round(max(distance_shortest_path_par_lock_scaled$distance_haversine)), 1)


expected_path_length_distance_par_lock_scaled <- data.frame(distance = dist_seq_par_lock_scaled, 
                                                     expected_path_length = sapply(dist_seq_par_lock_scaled, 
                                                                                   get_relative_shortest_path_length_by_distance,
                                                                                   baseline_mean_spl = average_shortest_path_t0,
                                                                                   epsilon = 1,
                                                                                   data = distance_shortest_path_par_lock_scaled)) %>%
  mutate(type = "Partial-lockdown rescaled",
         dash = "b")

ggplot() +
  geom_line(data = expected_path_length_distance_par_lock_scaled, aes(x = distance, y  = expected_path_length, group = 1))

# ------------------------ LOCKDOWN SCALED-----------------------------------
read(file = "week_53_scaled.Rdata")
mobility_graph_lock_scaled <- construct_graph_from_df(week_53_scaled)
shortest_paths_mobility_lock_scaled <- get_shortest_paths(mobility_graph_lock_scaled)

shortest_paths_df_lock_scaled <- data.frame(shortest_paths_mobility_lock_scaled)
colnames(shortest_paths_df_lock_scaled) <- rownames(shortest_paths_df_lock_scaled)

distance_shortest_path_lock_scaled <- shortest_paths_df_lock_scaled %>%
  rownames_to_column() %>%
  pivot_longer(-c("rowname")) %>%
  rename(from_name = rowname, to_name = name, shortest_path_length = value) %>%
  left_join(county_data, by = c("from_name" = "lk_movement")) %>%
  left_join(county_data, by = c("to_name" = "lk_movement")) %>%
  rename(from = lk_id.x, to = lk_id.y) %>%
  left_join(distances, by = c("from" = "from", "to" = "to")) %>%
  select(from = from_name, to = to_name, shortest_path_length, distance_haversine)




dist_seq_lock_scaled <- seq(0, round(max(distance_shortest_path_lock_scaled$distance_haversine)), 1)


expected_path_length_distance_lock_scaled <- data.frame(distance = dist_seq_lock_scaled, 
                                                            expected_path_length = sapply(dist_seq_lock_scaled, 
                                                                                          get_relative_shortest_path_length_by_distance,
                                                                                          baseline_mean_spl = average_shortest_path_t0,
                                                                                          epsilon = 1,
                                                                                          data = distance_shortest_path_lock_scaled)) %>%
  mutate(type = "Lockdown rescaled",
         dash = "b")

ggplot() +
  geom_line(data = expected_path_length_distance_lock_scaled, aes(x = distance, y  = expected_path_length, group = 1))

# ------------------------ COMBINED -----------------------------------
expected_path_length_distance_combined_scaled <- expected_path_length_distance_combined %>%
  dplyr::union(expected_path_length_distance_par_lock_scaled) %>%
  dplyr::union(expected_path_length_distance_lock_scaled)

ggplot() +
  geom_line(data = expected_path_length_distance_combined_scaled, aes(x = distance, y  = expected_path_length, group = type, color = type), alpha = 0.4, size = 1.2) + 
  geom_smooth(data = expected_path_length_distance_combined_scaled, aes(x = distance, y  = expected_path_length, group = type, color = type, linetype=type), size = 2) +
  ylab("Expected shortest path length \n (Relative to L of week 38)") +
  xlab("Distance d (km)") +
  scale_color_manual(values = c("#F8766D", "#fa9992", "#00BA38","#00932c", "#619CFF", "#619CFF")) +
  scale_fill_manual(values = c("#F8766D", "#fa9992", "#00BA38","#00932c", "#619CFF", "#619CFF")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) + 
  theme_bw() +
  guides(color=guide_legend(override.aes=list(fill='white'))) +
  theme(legend.position = c(0.225, 0.79),
        axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_blank(),
        legend.text = element_text(size=30),
        legend.key.size = unit(3,"line"),
        legend.key = element_rect(fill = NA))

+ theme(legend.background=element_blank())

