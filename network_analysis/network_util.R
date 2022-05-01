library(tidyverse)
library(igraph)
library(DirectedClustering)
library(ggplot2)

# ------------------------- FUNCTIONS -----------------------------
construct_graph <- function(filter_week){
  # INVERT EDGE WEIGHTS
  edges <- in_country_movements %>% # needs to be a global variable
    filter(week == filter_week) %>%
    select(from = kreis1, to = kreis2, weight = travellers_day) %>%
    mutate(weight = 1/weight) # invert weight to get the activation - lot of travel means short distance
  # CONVERT EDGE LIST INTO igraph
  return(graph_from_data_frame(edges, directed=TRUE))
}

construct_graph_from_df <- function(df){
  # INVERT EDGE WEIGHTS
  edges <- df %>%
    select(from, to, weight = scaled_travellers_day) %>%
    mutate(weight = 1/weight) # invert weight to get the activation - lot of travel means short distance
  # CONVERT EDGE LIST INTO igraph
  return(graph_from_data_frame(edges, directed=TRUE))
}

construct_graph_no_inverse <- function(filter_week){
  
  # INVERT EDGE WEIGHTS
  edges <- in_country_movements %>% # needs to be a global variable
    filter(week == filter_week) %>%
    select(from = kreis1, to = kreis2, weight = travellers_day) # %>%
    # mutate(weight = 1/weight) # invert weight to get the activation - lot of travel means short distance
  # CONVERT EDGE LIST INTO igraph
  return(graph_from_data_frame(edges, directed=TRUE))
}

get_shortest_paths <- function(graph){
  # CALCULATE SHORTEST PATHS
  return(distances(graph, mode = "out", algorithm = "dijkstra"))
}

# Get average shortest path
calculate_mean_shortest_path <- function(shortest_path_matrix){
  num_nodes <- nrow(shortest_path_matrix)
  return(sum(shortest_path_matrix) / (num_nodes*(num_nodes-1)))
}

# TODO could add standard deviation
get_relative_shortest_path_length_by_distance <- function(distance, baseline_mean_spl, epsilon, data){
  return((data %>%
            filter(distance_haversine >= distance - epsilon,
                   distance_haversine <= distance + epsilon) %>%
            summarise(mean_shortest_path_length = mean(shortest_path_length)))$mean_shortest_path_length / baseline_mean_spl)
}

calculate_global_cluster_coef <- function(graph){
  return(ClustF(as.matrix(as_adjacency_matrix(graph, attr = "weight")), type = "directed")$GlobaltotalCC)
}

haversine_dist <- function(lat1, long1, lat2, long2) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6371
  d <- R*c
  return(d)
}

get_prob_larger_dist <- function(distance, data){
  return((data %>%
            filter(distance_haversine >= distance) %>%
            summarise(count_edges = n()))$count_edges / nrow(data))
}
