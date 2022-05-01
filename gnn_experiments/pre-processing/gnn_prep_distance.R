library(tidyverse)
library(readr)

setwd("<<PATH TO THIS DIRECTORY>>")

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
  R <- 6378137
  d <- R*c
  return(d)
}

centroids <- read_csv("../../data/county_centroids.csv")

length(unique(centroids$lk_id))

centroids <- centroids %>%
  distinct(lk_id, .keep_all = TRUE) %>%
  filter(lk_id != 16056)

county_cross <- centroids %>%
  full_join(centroids, by=character())

distances <- county_cross %>%
  mutate(distance_euclid = sqrt((centroid_long.y - centroid_long.x)^2 + (centroid_lat.y - centroid_lat.x)^2),
         distance_haversine = haversine_dist(centroid_long.x, centroid_lat.x, centroid_long.y, centroid_lat.y)/1000) %>%
  select(lk_id.x, lk_id.y, distance_haversine, distance_euclid) %>%
  mutate(scaled_haversine = (distance_haversine - min(distance_haversine)) / (max(distance_haversine) - min(distance_haversine)),
         inverse_scaled_haversine = 1 - scaled_haversine) %>% # rounding to two digits to save space <- is this okay?
  select(lk_id.x, lk_id.y, inverse_scaled_haversine)


county_data <- read_csv("../../data/population_data.csv")

counties_with_movement <- read_csv("<<PATH TO ONE OF THE GNN EDGE LISTS>>", col_names = F) # these files have the counites with movements already filtered

distances_with_counties <- distances %>%
  left_join(county_data, by = c("lk_id.x" = "lk_id")) %>%
  left_join(county_data, by = c("lk_id.y" = "lk_id")) %>%
  select(start = lk_movement.x, start_id = lk_id.x, dest = lk_movement.y, dest_id = lk_id.y, inverse_scaled_haversine) %>%
  filter(start %in% unique(counties_with_movement$X1),
         dest %in% unique(counties_with_movement$X1))



write_csv(distances_with_counties %>%
            select(start, dest, inverse_scaled_haversine), 
            "<<PATH TO COUNTY DISTANCE FILE>>",
          col_names = FALSE) 
