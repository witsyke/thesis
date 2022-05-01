library(tidyverse)
library(readr)

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

centroids <- read_csv("<<PATH TO CENTROIDS>>")

length(unique(centroids$lk_id))

centroids <- centroids %>%
  distinct(lk_id, .keep_all = TRUE) %>%
  filter(lk_id != 16056)

county_cross <- centroids %>%
  full_join(centroids, by=character())

distances <- county_cross %>%
  mutate(distance_euclid = sqrt((centroid_long.y - centroid_long.x)^2 + (centroid_lat.y - centroid_lat.x)^2),
         distance_haversine = haversine_dist(centroid_long.x, centroid_lat.x, centroid_long.y, centroid_lat.y)/1000) %>%
  select(lk_id.x, lk_id.y, distance_haversine, distance_euclid)

#correlation
cases <- read_csv("<<PATH TO CASE DATA>>")

cases_only_lks <- cases %>%
  select(-week, -year)

sdtest <- sapply(cases_only_lks, sd)

which.min(sdtest)


counties <- read_csv("<<PATH TO POPULATION DATA>>") %>%
  select(lk_id, lk, bundesland)

cormat <- cor(cases_only_lks)
cormat[lower.tri(cormat)] <- NA

library(reshape2)
melted_cormat <- melt(cormat, na.rm = TRUE)

correlations <- as.data.frame(t(cormat))

correlations_long <- correlations %>%
  rownames_to_column("lk_id.x") %>%
  pivot_longer(-lk_id.x, names_to = "lk_id.y", values_to = "correlation_cases") %>%
  mutate(lk_id.x = as.double(lk_id.x),
         lk_id.y = as.double(lk_id.y))

correlations_long_na <- correlations_long

correlations_long <- correlations_long %>%
  filter(complete.cases(.))

distance_correlation <- distances %>%
  inner_join(correlations_long) %>%
  filter(correlation_cases != 1 & distance_haversine != 0)

cor(distance_correlation$distance_haversine, distance_correlation$correlation_cases)

ggplot(distance_correlation, aes(x=distance_haversine, y=correlation_cases)) + 
  geom_point(alpha = 0.1)

ggplot(distance_correlation, aes(x=distance_haversine, y=correlation_cases)) + 
  geom_hex(bins=100) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18)) 

correlations_long_lk_names <- correlations_long_na %>%
  left_join(counties, by=c("lk_id.x" = "lk_id")) %>%
  rename(lk.x = lk, bundesland.x = bundesland) %>%
  left_join(counties, by=c("lk_id.y" = "lk_id")) %>%
  rename(lk.y = lk, bundesland.y = bundesland) 
  

fillers <- tibble(lk_id = c(1063, 2001, 3463, 4013, 5979, 6637, 7341, 8438, 9781, 10047, 11001, 12074, 13077, 14731, 15092))

fillers.x <- fillers %>%
  full_join(counties, by=character()) %>%
  select(lk_id.x, lk_id.y) %>%
  mutate(correlation_cases = NA)
  
fillers.y = counties %>%
  select(lk_id, lk_id) %>%
  full_join(fillers, by=character()) %>%
  mutate(correlation_cases = NA)

  


correlations_long_lk_names %>%
  select(lk_id.x, lk_id.y, correlation_cases) %>%
  union(fillers.x) %>%
  union(fillers.y) %>%
  mutate(lk_id.x = as.factor(lk_id.x),
         lk_id.y = as.factor(lk_id.y)) %>%
  ggplot(., aes(lk_id.x, lk_id.y, fill=correlation_cases)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Pearson\nCorrelation", na.value="black") +
  theme(axis.text.x = element_blank(),
        axis.text.y =element_blank(),
        axis.title=element_text(size=18,face="bold"),
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18)) +
  coord_fixed() 

  
