# need the daily time series of movements -> similar to map plot 
# not sure if this will work too well because of the plareaus for each week -> might need to switch to weekly data


library(tidyverse)
library(ggplot2)
library(tsibble)

start_date = as.Date("2020-02-24")
end_date = as.Date("2021-10-10")

start_week <- paste(year = strftime(start_date, format = "%Y"), 
                    week = strftime(start_date, format = "%V"),
                    sep = "-")
end_week <- paste(year = strftime(end_date, format = "%Y"), 
                  week = strftime(end_date, format = "%V"),
                  sep = "-")

county_data <- read_csv("<<PATH TO POPULATION DATA>>")
total_population <- sum(county_data$population)

#-------------- GET CASE NUMBERS ----------------
cases <- read_csv("<<PATH TO ESTIMATED ACTIVE CASE DATA>>")

cases <- cases %>%
  mutate(IdLandkreis = replace(IdLandkreis, IdLandkreis %in% c(11001, 11002, 11003, 11004, 11005, 11006, 11007, 11008, 11009, 11010, 11011, 11012), 11000),
         IdLandkreis = replace(IdLandkreis, IdLandkreis == 16056, 16063)) %>%
  group_by(date, IdLandkreis) %>%
  summarise(AnzahlFall = sum(AnzahlFall)) %>%
  ungroup()


daily_cases_germany_norm <- cases %>%
  group_by(date, IdLandkreis) %>%
  summarise(total_active_cases = sum(AnzahlFall)) %>%
  mutate(total_active_cases_prop = total_active_cases / total_population) %>%
  left_join(county_data, by = c("IdLandkreis" = "lk_id")) %>%
  select(date, lk_movement, total_active_cases, total_active_cases_prop) %>%
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
  mutate(travellers = ifelse(is.na(travellers), 0, travellers)) %>%
  rename(loc.start = kreis1, loc.dest = kreis2) %>%
  filter(loc.start != loc.dest) # remove people that stay in county


# ---------- GATHERS MOVEMENT IN AND OUT OF EACH COUNTY -------------------
movements_out <- movements_to_other %>%
  group_by(week, loc.start) %>%
  summarise(sum_out = sum(travellers)) %>%
  ungroup() %>%
  select(week, loc = loc.start, sum = sum_out)

movements_in <- movements_to_other %>%
  group_by(week, loc.dest) %>%
  summarise(sum_in = sum(travellers)) %>%
  ungroup() %>%
  select(week, loc = loc.dest, sum = sum_in)


#-------------- CALCULATING DATES TO USE FOR PLOTTING OF MOVEMENT ----------------
date = seq(start_date, end_date, by="day")
dates = tibble(date)

match_dates <- dates %>%
  mutate(week = strftime(date, format = "%V"),
         year = strftime(date, format = "%Y"),
         match = paste(year, week, sep = "-"))

# --------------- COMBINE IN AND OUT MOVEMENTS AND AGGREGATE THEN ADD DAYS -----------

mean_movment <- movements_out %>%
  union(movements_in) %>%
  group_by(week, loc) %>%
  summarise(sum_comp = sum(sum) / 7) %>% # divide by 7 to get the daily number (should not change anything)
  ungroup() %>%
  left_join(match_dates, by = c("week" = "match")) %>%
  select(date, loc, mean_travellers = sum_comp)


# --------- COMBINE MOVEMENT INDICATOR WITH CASE NUMBERS ----------------
# time series of cases for each county
# join by day and county
# apply cross correlation to each county (does group_by work here?)
movement_cases <- mean_movment %>%
  left_join(daily_cases_germany_norm, by = c("date" = "date", "loc" = "lk_movement")) %>%
  mutate(total_active_cases = if_else(is.na(total_active_cases), 0, total_active_cases),
         total_active_cases_prop = if_else(is.na(total_active_cases_prop), 0, total_active_cases_prop)) %>%
  filter(date >= as.Date("2020-02-24")) 


# -------- LOOK AT SPECIFIC COUNTY ------------------
x <- movement_cases %>%
  # left_join(week_to_number) %>%
  filter(loc == "Hochtaunuskreis")
cor(x$mean_travellers, x$total_active_cases)
ggplot(x %>% pivot_longer(c("mean_travellers", "total_active_cases"), names_to = "type", values_to = "value"), aes(x = date, y = value, color = type)) +
  geom_line()
ccf(x$total_active_cases, x$mean_travellers, lag.max = 15, plot = T)

# TODO correlate with new cases as well 
# TODO to this with movements from and to other countries 
# TODO do this with weekly case data (mean active cases) -> done
# TODO do this with only first wave to compare to other --> done looks a lot more simular to what the paper shows 
# -> just that with what I have the strongest negative correlation is with no lag
# I thin now it is the correct way round with the lag


# -------- CALCULATE CROSS CORRELATION PER COUNTY ------------------

getCCF <- function(county){
  temp <- movement_cases %>%
    filter(loc == county)
  return(ccf(temp$total_active_cases, temp$mean_travellers, lag.max = 15, plot = FALSE)$acf)
}

y <- county_data %>%
  select(lk_id, lk_movement, bundesland) %>%
  arrange(lk_id) %>%
  rownames_to_column() %>%
  mutate(id = as.numeric(rowname)) %>%
  select(id, lk_id, lk_movement, bundesland)

cross_corr_per_county <- as.data.frame(sapply(unique(movement_cases$loc), getCCF)) %>%
  rownames_to_column() %>%
  mutate(lag = as.numeric(rowname) - 16) %>%
  filter(lag >= 0) %>%
  pivot_longer(-c("rowname", "lag"), names_to = "county", values_to = "corr") %>%
  select(-rowname) %>%
  left_join(county_data %>%
              select(lk_id, lk_movement) %>%
              arrange(lk_id) %>%
              rownames_to_column() %>%
              mutate(id = as.numeric(rowname)) %>%
              select(id, lk_id, lk_movement), by = c("county" = "lk_movement")) %>%
  arrange(id)
  



ggplot(cross_corr_per_county, aes(lag, id, fill= corr)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red")


# -------- PLOT ADVANCED CROSS CORRELATION HEATMAP (with clustering)------------------

library(ComplexHeatmap)
test1 <- cross_corr_per_county %>%
  select(county, lag, corr) %>%
  pivot_wider(id_cols = county, names_from = lag, values_from = corr)

corr_matrix <- matrix(cross_corr_per_county$corr, nrow = length(unique(cross_corr_per_county$county)), byrow = T)
rownames(corr_matrix) <- test1$county
colnames(corr_matrix) <- 0:15

Heatmap(corr_matrix, cluster_columns = FALSE)

