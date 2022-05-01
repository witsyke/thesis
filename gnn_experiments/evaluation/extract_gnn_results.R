# SCRIPT TO SEQUENTIALLY CONACTENATE RESULT FILES FOR A GNN EXPERIMENT
# the id has to be set to the folder name of the GNN experiment run
# this has to be done seperately for each GNN experiment
library(tidyverse)
setwd("<<PATH TO THIS DIRECTORY>>")


start_date <- as.Date("2020-09-01")
end_date <- as.Date("2020-12-31")

date <- seq(start_date, end_date, by ="day")
dates <- as_tibble(date) %>%
  rownames_to_column() %>%
  mutate(rowname = as.double(rowname) + 189) # only 14 because of 1 indexing

# Name of the GNN experiment that should be extracted
id <- "normal_mpnn_lstm_skip_ncm"

setwd(paste("<<PATH TO GNN EXPERIMENT OUTPUT>>", id, "/output/", sep = ""))
county_data <- read_csv("../../data/population_data.csv") %>%
  select(county = lk_movement, population)

file_names <- list.files(pattern = "*.csv")


get_day_data_shift_per_100k <- function(file_name){
  string <- str_split(file_name, "_")[[1]]
  time <- as.double(string[6]) # needs to be 7 and 8 for out only | 5 and 6 for mpnn
  shift <- as.double(str_split(string[7], ".csv")[[1]][1]) # little bit of a workaround to get the shift
  return(read_csv(file_name) %>%
           mutate(time = time,
                  time_shift = time + shift,
                  shift = shift) %>%
           select(county, real = observed, prediction, time, time_shift, shift) %>%
           left_join(county_data, by = c("county")) %>%
           mutate(experiment = "MPNN_LSTM_skip"))
}


results_shift <- file_names %>%
  map(get_day_data_shift_per_100k) 

results_shift_combined <- Reduce(rbind, results_shift)

results_shift_final <- results_shift_combined %>%
  left_join(dates, by = c("time_shift" = "rowname")) %>%
  mutate(start = as.character(date[time-189]),
         county = if_else(county == "Landkreis Nienburg/Weser", "Landkreis Nienburg_Weser", county)) %>%
  select(experiment, county, real, prediction, time, shift, population, date = value, start)

write_csv(results_shift_final, file = paste("<<PATH TO AGGREGATED GNN RESULTS>>", id, ".csv", sep = ""))

get_day_data_shift <- function(file_name){
  string <- str_split(file_name, "_")[[1]]
  time <- as.double(string[7]) # needs to be 7 and 8 for out only | 5 and 6 for mpnn
  shift <- as.double(str_split(string[8], ".csv")[[1]][1]) # little bit of a workaround to get the shift
  return(read_csv(file_name) %>%
           mutate(time = time,
                  time_shift = time + shift,
                  shift = shift) %>%
           select(county, real = observed, prediction, time, time_shift, shift) %>%
           left_join(county_data, by = c("county")) %>%
           mutate(prediction = round(prediction / 100000 * population),
                  real = round(real / 100000 * population),
                  experiment = "FEED_FORWARD + kappa"))
}


