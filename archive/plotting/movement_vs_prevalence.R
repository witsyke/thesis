library(tidyverse)
library(ggplot2)
library(tsibble)

start_date = as.Date("2020-01-21")
end_date = as.Date("2020-12-31")

start_week <- paste(year = strftime(start_date, format = "%Y"), 
                    week = strftime(start_date, format = "%V"),
                    sep = "-")
end_week <- paste(year = strftime(end_date, format = "%Y"), 
                  week = strftime(end_date, format = "%V"),
                  sep = "-")

county_data <- read_csv("<<PATH TO POPULATION DATA>>")
total_population <- sum(county_data$population)

#-------------- GET CASE NUMBERS ----------------
cases <- read_csv("<<PATH TO ESTIMATED ACTIVE CASES>>")

cases <- cases %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()

# TODO when refactoring don't forget empty cases

cases_germany_norm <- cases %>%
  group_by(date) %>%
  summarise(total_active_cases = sum(AnzahlFall)) %>%
  mutate(total_active_cases_prop = total_active_cases / total_population) %>%
  filter(date >= start_date,
         date <= end_date)



#-------------- LOAD MOVEMENTS ----------------
movements <- read_csv("<<PATH TO MOVEMENT DATA>>") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()


#-------------- INVESTIGATE DIFFERENCE IN DEVICE COUNTS ----------------
movements2 <- read_csv("<<PATH TO MOVEMENT DATA>>") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis"))

device_counts <- movements2 %>%
  group_by(week) %>%
  summarise(trav = sum(travellers),
            dev1 = sum(devices1, na.rm = T),
            dev2 = sum(devices2, na.rm = T)) %>%
  rownames_to_column() %>%
  pivot_longer(c("trav", "dev1", "dev2"), names_to = "type", values_to = "value")


ggplot(device_counts, aes(x = as.double(rowname), y = value, color = type)) +
  geom_line()


#-------------- CONTINUE LOADING NATIONAL MOVEMENTS ----------------
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
  rename(loc.start = kreis1, loc.dest = kreis2) %>%
  filter(loc.start != loc.dest, # remove people that stay in county
         mean_day_travellers > 5) # Remove all travels where mean travel per day is lower than 5

#-------------- CONTINUE LOADING INTERNATIONAL MOVEMENTS ----------------


movements_int <- movements %>%
  mutate(travellers = ifelse(!is.na(kreis1) & !is.na(kreis2), 0, travellers)) %>%
  mutate(kreis1 = ifelse(is.na(kreis1), staat1, kreis1),
         kreis2 = ifelse(is.na(kreis2), staat2, kreis2)) %>%
  filter(kreis1 != "Germany",
         kreis2 != "Germany")


# 1.1. Generate possible movements
# Extract unqiue start/ destination locations
unique_locations <- movements_int %>%
  select(loc = kreis1) %>%
  union(movements %>%
          select(loc = kreis2)) %>%
  unique()

german_counties <- movements %>%
  filter(complete.cases(.)) %>%
  select(kreis1) %>%
  distinct()

# Generate movement for each start-destination-week combination
movement_weeks_int <- unique_locations %>%
  full_join(unique_locations, by = character(), suffix = c(".start", ".dest")) %>%
  full_join(movements_int %>%
              select(week) %>%
              filter(week >= start_week,
                     week <= end_week) %>%
              unique(), by = character())


movements_filtered_int <- movements_int %>%
  filter(week >= start_week,
         week <= end_week)


# Add actual movements to data frame with possible movements
# Add 0 movements
movements_to_other_int <- movement_weeks_int %>%
  left_join(movements_filtered_int, by = c("loc.start" = "kreis1", "loc.dest" = "kreis2", "week")) %>%
  mutate(travellers = ifelse(is.na(travellers), 0, travellers),
         mean_day_travellers = travellers / 7) %>%
  filter(loc.start != loc.dest,
         !(loc.start %in% german_counties$kreis1 & loc.dest %in% german_counties$kreis1),
         !(!(loc.start %in% german_counties$kreis1) & !(loc.dest %in% german_counties$kreis1)), 
         mean_day_travellers > 5) # remove people that stay in county




# ------------------ CHECK THE DIFFERENCE IN THE SUM OF TRAVELLERS BETWEEN THRESHOLD AND NOT THRESHOLD --------------------
movements_to_other_no_threshold <- movement_weeks %>%
  left_join(in_country_movements, by = c("kreis1", "kreis2", "week")) %>%
  filter(complete.cases(.)) %>%
  mutate(travellers = ifelse(is.na(travellers), 0, travellers),
         mean_day_travellers = travellers / 7) %>%
  rename(loc.start = kreis1, loc.dest = kreis2) %>%
  filter(loc.start != loc.dest) # remove people that stay in county

movements_to_other %>%
  group_by(week) %>%
  summarise(sum_trav = sum(travellers),
            sum_mean_trav = sum(mean_day_travellers)) %>%
  left_join(movements_to_other_no_threshold %>%
              group_by(week) %>%
              summarise(sum_trav = sum(travellers),
                        sum_mean_trav = sum(mean_day_travellers)), by = c("week")) %>%
  mutate(change = (sum_trav.x/sum_trav.y)-1,
         change_day = (sum_mean_trav.x/sum_mean_trav.y)-1) %>%
  summarise(mean_change = mean(change),
            min_change = max(change),
            max_change = min(change),
            median_change = median(change))




#-------------- CHECK IF ANY COUNTIES ARE MISSING IN SUMMER 2021 ----------------
# Look if I'm missing counties in 2021
test3 <- movements_to_other %>%
  filter(week %in% c("2021-28", "2021-29", "2021-30", "2021-31", "2021-32")) %>%
  select(loc.start, loc.dest) %>%
  pivot_longer(c("loc.start", "loc.dest"), names_to = "type", values_to = "loc")

length(unique(test3$loc)) # Looks like all the counties are there

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


movements_sum_norm_int <- movements_to_other_int %>%
  group_by(week) %>%
  summarise(sum_travellers = sum(travellers),
            sum_travellers_prop = sum(travellers)/total_population,
            sum_day_travellers = sum(mean_day_travellers),
            sum_day_travellers_prop = sum(mean_day_travellers)/total_population) %>%
  left_join(match_dates, by = c("week" = "match"))
  
#-------------- PLOT MOVEMENTS VS CASE NUMBERS ----------------
# TODO could try min-max scaling for both (but use the same max) - Talk to pascal
ggplot() +
  geom_line(data = movements_sum_norm, aes(x = date, y = sum_day_travellers)) +
  geom_line(data = cases_germany_norm, aes(x = date, y = total_active_cases_prop*100000000), color = "red") +
  scale_y_continuous(
    "Travellers (%)", 
    sec.axis = sec_axis(~ . / 100000000, name = "Prevalence (%)")
  ) +
  scale_x_date(date_breaks="4 weeks", date_labels = "%m/%d") +
  theme(title = element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        axis.text = element_text(size=18),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_blank(),
        axis.title.y.right = element_text(color = "red"),
        axis.text.y.left = element_blank())

# ggplot() +
#   # geom_line(data = movements_sum_norm, aes(x = date, y = sum_day_travellers)) +
#   geom_line(data = movements_sum_norm_int, aes(x = date, y = sum_day_travellers)) +
#   geom_line(data = cases_germany_norm, aes(x = date, y = total_active_cases_prop*10000000), color = "red") +
#   scale_y_continuous(
#     "Travellers (%)", 
#     sec.axis = sec_axis(~ . / 100000000, name = "Prevalence (%)")
#   ) +
#   scale_x_date(date_breaks="4 weeks", date_labels = "%m/%d") +
#   theme(title = element_text(size=20,face="bold"),
#         axis.title=element_text(size=20,face="bold"),
#         axis.text = element_text(size=18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18),
#         axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1),
#         axis.line.y.right = element_line(color = "red"), 
#         axis.ticks.y.right = element_line(color = "red"),
#         axis.text.y.right = element_blank(),
#         axis.title.y.right = element_text(color = "red"),
#         axis.text.y.left = element_blank())
