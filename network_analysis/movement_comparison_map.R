library(scales)
library(raster)
library(tidyverse)
library(ggplot2)

setwd("<<PATH TO THIS DIRECTORY>>")
source("extract_movements_per_county.R")

# -------- LOAD COUNTY DATA ---------------
county_data <- read_csv("../data/population_data.csv")


# -------- LOAD MOVEMENTS ---------------

movements <- read_csv("../sir_experiments/data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

# Set wave for plotting
movement_type = "all"

# -------- DATA FOR PERIOD 1 OF COMPARISION ---------------
# 1. Filter dates
# start_date = "2020-09-19"
# end_date = "2020-09-24"
start_week <- "2020-38"
end_week <- "2020-38"


# -------- DATA FOR PERIOD 2 OF COMPARISION ---------------
# start_date2 = "2020-03-11"
# end_date2 = "2020-04-13"

start_week2 <- "2020-53"
end_week2 <- "2020-53"


# ---------- GET TOTAL MOVEMENTS PER COUNTY ---------------

mean_movment_t1 <- extract_total_movement_per_county_single_week(start_week, movements, movement = movement_type, include_local = T)
mean_movment_t2 <- extract_total_movement_per_county_single_week(start_week2, movements, movement = movement_type, include_local = T)


#------------------- CALCULATE CHANGE IN MOVEMENTS ---------------------
movement_change <- mean_movment_t1 %>%
  left_join(mean_movment_t2, by = c("kreis" = "kreis"), suffix = c(".t1", ".t2")) %>%
  mutate(movement_change = (mean.t2 / mean.t1) - 1,
         movment_prop = mean.t2 / mean.t1,
         movment_change_perc = movement_change * 100,
         movment_prop_perc = movment_prop * 100)


# Need to rename some counties in this hacky way to match them to map 
# TODO if there is time at the end this could be replaced by the shapefile with the ids
movement_change_final <- movement_change %>%
  left_join(county_data, by = c("kreis" = "lk_movement")) %>%
  mutate(lk = gsub("kreisfreie Stadt", "", lk),
         lk = gsub("Stadt der FernUniversität", "", lk),
         lk = gsub("Stadt ", "", lk),
         lk = gsub("Freie und Hansestadt", "", lk),
         lk = gsub("Wissenschaftsstadt", "", lk),
         lk = gsub("Hansestadt", "", lk),
         lk = gsub("Stadtkreis ", "", lk),
         lk = gsub("Landeshauptstadt", "", lk),
         lk = gsub("Klingenstadt", "", lk),
         lk = trimws(lk, which = "left"))

#------------------- GET MAP AND COMBINE WITH DATA ---------------------
# get spatial data for Germany on county level
germany <- getData(country = "Germany", level = 2) 
germany@data$id <- rownames(germany@data)


map_with_data <- fortify(germany) %>% 
  left_join(germany@data, by = "id") %>%
  mutate(NAME_2 = if_else(NAME_2 == "Osterode am Harz", "Göttingen", NAME_2),
         NAME_2 = if_else(NAME_2 == "Eisenach", "Wartburgkreis", NAME_2),
         NAME_2 = if_else(grepl("chen (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("rzburg (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("rth (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("Osna", NAME_2, fixed = T) & grepl("ck (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2)) %>%
  left_join(movement_change_final, by = c("NAME_2" = "lk"))

#------------------ PLOT MAP --------------------
ggplot(data = map_with_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=movement_change), colour = "white", size = 0.01) +
  coord_map() +
  scale_fill_continuous(low="#de2d26", high="#fee8c8",
                         guide="colorbar",na.value="white", name = "Movement \n change", labels = percent, limits = c(-0.9, -0.1)) +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18),
        legend.key.size = unit(1,"line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

