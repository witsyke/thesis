library(tidyverse)
library(readr)
library(gridExtra)

setwd("<<PATH TO MOVEMENT DIRECTORY>>")
file_names <- list.files(pattern = "*.csv")

data2021 <- file_names %>%
  map(read_csv) %>%
  reduce(union)
  # reduce(union) %>%
  # mutate(kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
  #        kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis"))

setwd("<<PATH TO RAW MOVEMENT DATA>>")
file_names <- list.files(pattern = "*.csv")
data2020 <- file_names %>%
  map(read_csv) %>%
  reduce(union)

data <- data2020 %>%
  union(data2021)

write_csv(data, file = gzfile("../complete_movement.csv.gz"))

data <- read_csv("../complete_movement.csv.gz")

ggplot(data, aes(x = prop_devices_population2)) +
  geom_density()

ggplot(data, aes(x = prop_devices_population1)) +
  geom_density()

ggplot(data, aes(x = devices2)) +
  geom_density()

data_norm <- data %>%
  mutate(travellers_norm = travellers / (devices2 *  population2) * 100000)


test <- data_norm %>%
  group_by(kreis2) %>%
  summarise(mean_travel = mean(travellers_norm)) %>%
  arrange(-mean_travel) %>%
  filter(!grepl("Berlin", kreis2, fixed = TRUE)) %>%
  left_join(counties, by=c("kreis2" = "lk_movement")) %>%
  select(lk_id, mean_travel, lk, bundesland, population)

week_dest_county <- data_norm %>%
  group_by(week, kreis2) %>%
  summarise(sum_movement = sum(travellers_norm, na.rm = T))

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outliers <- week_dest_county %>%
  filter(is_outlier(sum_movement)) %>%
  mutate(week = as.factor(week),
         kreis2 = as.factor(kreis2))

summary(outliers)

# distribution of total (sum) movements towards destination counties
week_dest_county %>%
  mutate(outlier = ifelse(is_outlier(sum_movement), kreis2, "-")) %>%
  ggplot(., aes(x = week, y = sum_movement)) +
    geom_boxplot() +
    # geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) 


week <- (data_norm %>%
  group_by(week) %>%
  summarise(sum_movement = sum(travellers_norm, na.rm = T),
            mean_movement_by_week = sum(travellers_norm, na.rm = T) / 7,
            mean_movement = mean(travellers_norm, na.rm = T),
            count_movement = n()))

week <- data_norm %>%
  group_by(week) %>%
  summarise(sum_movement = sum(travellers_norm, na.rm = T))

# does it really make sense here to take the mean over all movements?
# --> wouldn't this complicate interpretability (number of movements would be eliminated)
# taking the mean would mean we make a single movement for each week comparable
# --> interested in total movement per week

p1 <- ggplot(week, aes(x = as.factor(week), group = 1)) +
  # geom_line(aes(y=sum_movement)) +
  geom_line(aes(y = mean_movement)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18,face="bold")) 

p2 <- ggplot(week, aes(x = as.factor(week), group = 1)) +
  geom_line(aes(y = count_movement)) +
  # geom_line(aes(y=count_movement)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18,face="bold")) 

p3 <- ggplot(week, aes(x = as.factor(week), group = 1)) +
  geom_line(aes(y = sum_movement)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18,face="bold")) 

p4 <- ggplot(week, aes(x = as.factor(week), group = 1)) +
  geom_line(aes(y = mean_movement_by_week)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size=16),
        axis.title = element_text(size=18,face="bold")) 

grid.arrange(p1, p2, p3, ncol = 3)

counties <-  read_csv("../data/population_data.csv")

unique(data$kreis2)

test <- head(data)

library(ggplot2)
