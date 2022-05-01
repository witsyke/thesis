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
 

print(paste("Total number of movements in week", filter_week, ":", nrow(in_country_movements)))


# ------------------------ SHORTEST PATH LENGTH -----------------------------------
mobility_graph <- construct_graph("2020-10")
shortest_paths_mobility <- get_shortest_paths(mobility_graph)
average_shortest_path <- calculate_mean_shortest_path(shortest_paths_mobility)


# ------------------------ CLUSTERING COEFFICENT -----------------------------------
average_clustering_coef <- calculate_global_cluster_coef(mobility_graph)

mobility_graph2 <- construct_graph_no_inverse("2020-10")
average_clustering_coef2 <- calculate_global_cluster_coef(mobility_graph2)

# ------------------------  CHANGE -----------------------------------
weeks <- unique(movements$week)[33:53] # TODO this is ugly but works
mobility_graph_list <- lapply(weeks, construct_graph)
# ------- SHORTEST PATH LENGTH
shortest_paths_mobility_list <- lapply(mobility_graph_list, get_shortest_paths)
average_shortest_paths <- sapply(shortest_paths_mobility_list, calculate_mean_shortest_path)

mean_shortest_path_week <- data.frame(week = weeks, mean_shortest_path_length = average_shortest_paths)

ggplot(mean_shortest_path_week) +
  geom_line(aes(x = week, y = mean_shortest_path_length, group = 1)) +
  geom_point(aes(x = week, y = mean_shortest_path_length), shape = 18, size = 3) + 
  theme(title = element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        axis.text = element_text(size=18),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        axis.text.y.left = element_blank())

# ------- CLUSTERING COEFFICENT
mobility_graph_list2 <- lapply(weeks, construct_graph_no_inverse)
mean_cluster_coef <- sapply(mobility_graph_list2, calculate_global_cluster_coef)

mean_cluster_coef_week <- data.frame(week = weeks, mean_cluster_coef = mean_cluster_coef)

ggplot(mean_cluster_coef_week) +
  geom_line(aes(x = week, y = mean_cluster_coef, group = 1)) +
  geom_point(aes(x = week, y = mean_cluster_coef), shape = 16, size = 3) + 
  theme(title = element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        axis.text = element_text(size=18),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        axis.text.y.left = element_blank())


# ------------------------ RELATIVE CHANGE -----------------------------------

sp_relative_change <- mean_shortest_path_week %>%
  mutate(mean_shortest_path_t0 = (mean_shortest_path_week %>%
           filter(week == "2020-38"))$mean_shortest_path_length,
         sp_relative_change = mean_shortest_path_length / mean_shortest_path_t0) %>%
  select(week, sp_relative_change)

cc_relative_change <- mean_cluster_coef_week %>%
  mutate(mean_cluster_coef_t0 = (mean_cluster_coef_week %>%
           filter(week == "2020-38"))$mean_cluster_coef,
         cc_relative_change = mean_cluster_coef / mean_cluster_coef_t0) %>%
  select(week, cc_relative_change)

# complete_relative_change <- sp_relative_change %>%
#   left_join(cc_relative_change, by = c("week")) %>%
#   pivot_longer(c("sp_relative_change", "cc_relative_change"), names_to = "type", values_to = "change")

complete_relative_change <- sp_relative_change %>%
  pivot_longer(c("sp_relative_change"), names_to = "type", values_to = "change") %>%
  separate(week, c("year", "week"), sep = "-")


ggplot(complete_relative_change) +
  geom_line(aes(x = week, y = change, group = type), size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  geom_vline(xintercept = "38", size = 1.5, color="#619CFF") +
  geom_vline(xintercept = "47", size = 1.5, color="#00BA38") +
  geom_vline(xintercept = "53", size = 1.5, color="#F8766D") +
  geom_point(aes(x = week, y = change), shape = 16, size = 7) + 
  xlab("Week") +
  ylab("Relative change of L (to week 38)") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_text(size=34,face="bold"),
        legend.text = element_text(size=30))

ggplot(complete_relative_change %>% mutate(week = as.numeric(week)) %>% filter(week >= 38)) +
  # geom_rect(data = tibble(), aes(xmin=37.8, xmax=45.2, ymin=-Inf, ymax=Inf), fill="darkgreen", alpha = 0.5) +
  geom_rect(data = tibble(), aes(xmin=51.8, xmax=53.2, ymin=-Inf, ymax=Inf), fill="darkgreen", alpha = 0.5) +
  geom_rect(data = tibble(), aes(xmin=45.8, xmax=51.2, ymin=-Inf, ymax=Inf), fill="red", alpha = 0.5) +
  geom_line(aes(x = week, y = change, group = type), size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  # geom_vline(xintercept = "38", size = 1.5, color="#619CFF") +
  # geom_vline(xintercept = "47", size = 1.5, color="#00BA38") +
  # geom_vline(xintercept = "53", size = 1.5, color="#F8766D") +
  geom_point(aes(x = week, y = change), shape = 16, size = 7) + 
  scale_x_continuous(labels = as.character(33:53), breaks = 33:53) +
  xlab("Week") +
  ylab("Relative change of L (to week 38)") + 
  theme_bw() +
  theme(axis.title=element_text(size=34,face="bold"),
        axis.text = element_text(size=30),
        legend.title = element_text(size=34,face="bold"),
        legend.text = element_text(size=30))





