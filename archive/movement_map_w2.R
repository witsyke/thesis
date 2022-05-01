library(tidyverse)


setwd("<<PATH TO SIR EXPERIEMENTS>>")

movement_data <- read_csv("data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

# 1. Filter dates

start_date = "2020-06-10"
end_date = "2020-09-15"
start_week <- paste(year = strftime(start_date, format = "%Y"), 
                    week = strftime(start_date, format = "%V"),
                    sep = "-")
end_week <- paste(year = strftime(end_date, format = "%Y"), 
                  week = strftime(end_date, format = "%V"),
                  sep = "-")

# 1.1. Building full empty matrix for movements for all counties to each other
movement_weeks <- movement_data %>%
  filter(complete.cases(.)) %>%
  select(kreis1) %>%
  unique() %>%
  full_join(movement_data %>%
              filter(complete.cases(.)) %>%
              select(kreis2) %>%
              unique(), by=character()) %>%
  full_join(movement_data %>% select(week) %>%  
              filter(week >= start_week, 
                     week <= end_week) %>% 
              unique(), by = character())

movements_filtered <- movement_data %>%
  filter(week >= start_week,
         week <= end_week)
print(paste("Total number of movements from", start_week, "to", end_week, ":", nrow(movements_filtered)))

# 2. Extract in country movements 
in_country_movements <- movements_filtered %>%
  filter(complete.cases(.)) %>%
  select(week, kreis1, kreis2, travellers) %>%
  filter(kreis1 != kreis2)


movements_out <- in_country_movements %>%
  group_by(week, kreis1) %>%
  summarise(sum_out = sum(travellers)) %>%
  ungroup() %>%
  select(week, kreis = kreis1, sum = sum_out)

movements_in <- in_country_movements %>%
  group_by(week, kreis2) %>%
  summarise(sum_in = sum(travellers)) %>%
  ungroup() %>%
  select(week, kreis = kreis2, sum = sum_in)

mean_movment_t1 <- movements_out %>%
  union(movements_in) %>%
  group_by(week, kreis) %>%
  summarise(sum_comp = sum(sum)) %>%
  group_by(kreis) %>%
  summarise(mean = mean(sum_comp)) %>%
  ungroup()

#------------------
start_date = "2020-09-16"
end_date = "2020-12-31"

start_week <- paste(year = strftime(start_date, format = "%Y"), 
                    week = strftime(start_date, format = "%V"),
                    sep = "-")
end_week <- paste(year = strftime(end_date, format = "%Y"), 
                  week = strftime(end_date, format = "%V"),
                  sep = "-")

# 1.1. Building full empty matrix for movements for all counties to each other
movement_weeks <- movement_data %>%
  filter(complete.cases(.)) %>%
  select(kreis1) %>%
  unique() %>%
  full_join(movement_data %>%
              filter(complete.cases(.)) %>%
              select(kreis2) %>%
              unique(), by=character()) %>%
  full_join(movement_data %>% select(week) %>%  
              filter(week >= start_week, 
                     week <= end_week) %>% 
              unique(), by = character())

movements_filtered <- movement_data %>%
  filter(week >= start_week,
         week <= end_week)
print(paste("Total number of movements from", start_week, "to", end_week, ":", nrow(movements_filtered)))

# 2. Extract in country movements 
in_country_movements <- movements_filtered %>%
  filter(complete.cases(.)) %>%
  select(week, kreis1, kreis2, travellers) %>%
  filter(kreis1 != kreis2)


movements_out <- in_country_movements %>%
  group_by(week, kreis1) %>%
  summarise(sum_out = sum(travellers)) %>%
  ungroup() %>%
  select(week, kreis = kreis1, sum = sum_out)

movements_in <- in_country_movements %>%
  group_by(week, kreis2) %>%
  summarise(sum_in = sum(travellers)) %>%
  ungroup() %>%
  select(week, kreis = kreis2, sum = sum_in)

mean_movment_t2 <- movements_out %>%
  union(movements_in) %>%
  group_by(week, kreis) %>%
  summarise(sum_comp = sum(sum)) %>%
  group_by(kreis) %>%
  summarise(mean = mean(sum_comp)) %>%
  ungroup()


#-------------------
movment_change <- mean_movment_t1 %>%
  left_join(mean_movment_t2, by = c("kreis" = "kreis"), suffix = c(".t1", ".t2")) %>%
  mutate(movement_change = (mean.t2 / mean.t1) - 1,
         movment_prop = mean.t2 / mean.t1,
         movment_change_perc = movement_change * 100,
         movment_prop_perc = movment_prop * 100)

county_data <- read_csv("<<PATH TO POPULATION DATA>>")

library(dyplr)
test <- movment_change %>%
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



library(broom)
lol <- fortify(germany) %>% 
  left_join(germany@data, by = "id") %>%
  mutate(NAME_2 = if_else(grepl("chen (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("rzburg (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("rth (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2),
         NAME_2 = if_else(grepl("Osna", NAME_2, fixed = T) & grepl("ck (Kreis", NAME_2, fixed = T), "München (Kreisfreie Stadt)", NAME_2)) %>%
  left_join(test, by = c("NAME_2" = "lk"))

view(head(lol %>%
            arrange(movment_change_perc)))


fortify(germany)

germany@data$id <- rownames(germany@data)

#------------------

# get spatial data for Germany on county level
library(raster)
library(ggplot2)
library(scales)

germany <- getData(country = "Germany", level = 2) 


ggplot(data = lol, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=movement_change), colour = "white", size = 0.03) +
  coord_map() +
  scale_fill_continuous(low="#de2d26", high="#fee8c8",
                        guide="colorbar",na.value="white", name = "Change in Movements", labels = percent) +
  theme_bw() +
  ggtitle("Change in movements", subtitle = "pre vs during wave 2") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(size = 24),
        legend.text = element_text(size = 20))
 
