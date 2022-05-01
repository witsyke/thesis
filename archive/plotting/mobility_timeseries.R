library(tidyverse)
library(ggplot2)
library(tsibble)

start_date = as.Date("2020-01-21")
end_date = as.Date("2021-10-01")

start_week <- paste(year = strftime(start_date, format = "%Y"), 
                    week = strftime(start_date, format = "%V"),
                    sep = "-")
end_week <- paste(year = strftime(end_date, format = "%Y"), 
                  week = strftime(end_date, format = "%V"),
                  sep = "-")

county_data <- read_csv("<<PATH TO POPULATION DATA>>")
total_population <- sum(county_data$population)



#-------------- LOAD MOVEMENTS ----------------
movements <- read_csv("<<PATH TO MOVEMENT DATA>>") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

movement_weeks <- movements %>%
  filter(complete.cases(.)) %>%
  select(kreis1) %>%
  unique() %>%
  full_join(movements %>%
              filter(complete.cases(.)) %>%
              select(kreis2) %>%
              unique(), by=character()) %>%
  full_join(movements %>% select(week) %>%  
              filter(week >= start_week, 
                     week <= end_week) %>% 
              unique(), by = character())

movements_filtered <- movements %>%
  filter(week >= start_week,
         week <= end_week)

# 2. Extract in country movements 
in_country_movements <- movements_filtered %>%
  filter(complete.cases(.)) %>%
  select(week, kreis1, kreis2, travellers)

# Add actual movements to data frame with possible movements
# Add 0 movements
movements_to_other <- movement_weeks %>%
  left_join(in_country_movements, by = c("kreis1", "kreis2", "week")) %>%
  mutate(travellers = ifelse(is.na(travellers), 0, travellers),
         mean_day_travellers = travellers / 7) %>%
  rename(loc.start = kreis1, loc.dest = kreis2)





#-------------- CALCULATING DATES TO USE FOR PLOTTING OF MOVEMENT ----------------
date = seq(start_date, end_date, by="day")
dates = tibble(date)

match_dates <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-"))

movements_sum_norm <- movements_to_other %>%
  group_by(week) %>%
  summarise(sum_travellers = sum(travellers),
            sum_travellers_prop = sum(travellers)/total_population,
            sum_day_travellers = sum(mean_day_travellers),
            sum_day_travellers_prop = sum(mean_day_travellers)/total_population) %>%
  left_join(match_dates, by = c("week" = "match"))


#-------------- PLOT MOVEMENTS VS CASE NUMBERS ----------------
# TODO could try min-max scaling for both (but use the same max) - Talk to pascal
ggplot() +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed", size = 1) +
  geom_line(data = movements_sum_norm, aes(x = date, y = sum_day_travellers), size = 1) +
  scale_x_date(date_breaks="1 months", date_labels = "%m-%y") +
  xlab("Day") +
  ylab("Average movements") +
  theme_bw() +
  theme(axis.title=element_text(size=22,face="bold"),
        axis.text = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))


