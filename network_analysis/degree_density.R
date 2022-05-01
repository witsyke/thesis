# CALCULATES AND VISUALIZES THE DEGREE, STRENGTH AND DENSITY of THE PRE-LOCKDOWN, PARTIAL LOCKDOWN, AND EXTENDED LOCKDOWN WEEK
# # Requires mobility data that is not publicly available.
library(tidyverse)
library(igraph)
library(ggplot2)
library(EnvStats)
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


# -------------------- PRE-LOCKDOWN ---------------------------
# ------ DEGREE DISTRIBUTION 
graph_pre <- construct_graph_no_inverse("2020-38")

node_degree_per_county_pre <- data.frame(as.list(degree(graph_pre, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_degree")

round(median(node_degree_per_county_pre$node_degree), digits = 1)
round(mean(node_degree_per_county_pre$node_degree), digits = 1)

ggplot(node_degree_per_county_pre) +
  geom_histogram(aes(x = node_degree), bins = 80, fill = "#619CFF") +
  geom_vline(data = node_degree_per_county_pre %>% 
               summarise(Mean = mean(node_degree),
                         Median = median(node_degree)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.8) + 
  theme_bw() +
  xlab("Vertex degree k") +
  ylab("Count") +
  xlim(60,400) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.125,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38))

node_strength_per_county_pre <- data.frame(as.list(strength(graph_pre, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_strength")

round(median(node_strength_per_county_pre$node_strength), digits = 1)
round(mean(node_strength_per_county_pre$node_strength), digits = 1)

ggplot(node_strength_per_county_pre) +
  geom_histogram(aes(x = node_strength), bins = 50, fill = "#619CFF") +
  geom_vline(data = node_strength_per_county_pre %>% 
               summarise(Mean = mean(node_strength),
                         Median = median(node_strength)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.75) + 
  theme_bw() +
  xlab("Vertex strength s") +
  ylab("Count") +
  xlim(0, 10000) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.865,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38),
        plot.margin = margin(5.5, 22, 5.5, 5.5))

# ------ DENSITY 
density_pre <- edge_density(graph_pre, loops = FALSE)     

in_country_movements %>%
  filter(week == "2020-38") %>%
  summarise(mean_travellers = mean(travellers),
            cv_travellers = cv(travellers),
            max_travllers = max(travellers),
            min_travellers = min(travellers),
            sd_travellers = sd(travellers),
            var_travellers = var(travellers))


# -------------------- PARTIAL LOCKDOWN ---------------------------
# ------ DEGREE DISTRIBUTION 
graph_par_lock <- construct_graph_no_inverse("2020-47")

node_degree_per_county_par_lock <- data.frame(as.list(degree(graph_par_lock, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_degree")

round(median(node_degree_per_county_par_lock$node_degree), digits = 1)
round(mean(node_degree_per_county_par_lock$node_degree), digits = 1)

ggplot(node_degree_per_county_par_lock) +
  geom_histogram(aes(x = node_degree), bins = 80, fill = "#00BA38") +
  geom_vline(data = node_degree_per_county_par_lock %>% 
               summarise(Mean = mean(node_degree),
                         Median = median(node_degree)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.75) + 
  theme_bw() +
  xlab("Vertex degree k") +
  ylab("Count") +
  xlim(60,400) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.125,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38))

node_strength_per_county_par_lock <- data.frame(as.list(strength(graph_par_lock, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_strength")

round(median(node_strength_per_county_par_lock$node_strength), digits = 1)
round(mean(node_strength_per_county_par_lock$node_strength), digits = 1)

ggplot(node_strength_per_county_par_lock) +
  geom_histogram(aes(x = node_strength), bins = 50, fill = "#00BA38") +
  geom_vline(data = node_strength_per_county_par_lock %>% 
               summarise(Mean = mean(node_strength),
                         Median = median(node_strength)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.75) + 
  theme_bw() +
  xlab("Vertex strength s") +
  ylab("Count") +
  xlim(0, 10000) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.865,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38),
        plot.margin = margin(5.5, 22, 5.5, 5.5))

# ------ DENSITY 
density_par_lock <- edge_density(graph_par_lock, loops = FALSE)   

in_country_movements %>%
  filter(week == "2020-47") %>%
  summarise(mean_travellers = mean(travellers),
            cv_travellers = cv(travellers),
            max_travllers = max(travellers),
            min_travellers = min(travellers),
            sd_travellers = sd(travellers),
            var_travellers = var(travellers))

# -------------------- LOCKDOWN ---------------------------
# ------ DEGREE DISTRIBUTION 
graph_lock <- construct_graph_no_inverse("2020-53")

node_degree_per_county_lock <- data.frame(as.list(degree(graph_lock, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_degree")

round(median(node_degree_per_county_lock$node_degree), digits = 1)
round(mean(node_degree_per_county_lock$node_degree), digits = 1)

ggplot(node_degree_per_county_lock) +
  geom_histogram(aes(x = node_degree), bins = 80, fill = "#F8766D") +
  geom_vline(data = node_degree_per_county_lock %>% 
               summarise(Mean = mean(node_degree),
                         Median = median(node_degree)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.75) + 
  theme_bw() +
  xlab("Vertex degree k") +
  ylab("Count") +
  xlim(60,400) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.125,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38))

node_strength_per_county_lock <- data.frame(as.list(strength(graph_lock, mode = "in"))) %>%
  pivot_longer(everything(), names_to = "county", values_to = "node_strength")

round(median(node_strength_per_county_lock$node_strength), digits = 1)
round(mean(node_strength_per_county_lock$node_strength), digits = 1)

ggplot(node_strength_per_county_lock) +
  geom_histogram(aes(x = node_strength), bins = 50, fill = "#F8766D", size=0) +
  geom_vline(data = node_strength_per_county_lock %>% 
               summarise(Mean = mean(node_strength),
                         Median = median(node_strength)) %>%
               pivot_longer(everything()),
             aes(xintercept=value, linetype = name), size = 1.75) + 
  theme_bw() +
  xlab("Vertex strength s") +
  ylab("Count") +
  xlim(0, 10000) +
  theme(axis.title=element_text(size=40,face="bold"),
        axis.text = element_text(size=38),
        legend.position = c(0.865,0.905),
        legend.title = element_blank(),
        legend.text = element_text(size=38),
        plot.margin = margin(5.5, 22, 5.5, 5.5))

 # ------ DENSITY 
density_lock <- edge_density(graph_lock, loops = FALSE)     

in_country_movements %>%
  filter(week == "2020-53") %>%
  summarise(mean_travellers = mean(travellers),
            cv_travellers = cv(travellers),
            max_travllers = max(travellers),
            min_travellers = min(travellers),
            sd_travellers = sd(travellers),
            var_travellers = var(travellers))
