# Load libraries
library(tidyverse)
library(ggplot2)

setwd("<<PATH TO INTERNATIONAL DATA>>")

# Read case, recovery and death data
cases <- read_csv("<<INTERNATIONAL CASES>>")
recoveries <- read_csv("<<INTERNATIONAL RECOVERIES>>")
deaths <- read_csv("INTERNATIONAL DEATHS")

# Basic data cleaning function
aggregate_countries <- function(data_frame) {
    return(data_frame %>%
        filter(`Country/Region` != "#country+name") %>%
        group_by(`ISO 3166-1 Alpha 3-Codes`) %>%
        summarise_if(is.numeric, sum))
}

# Basic data cleaning
cases_by_country <- aggregate_countries(cases)
recoveries_by_country <- aggregate_countries(recoveries)
deaths_by_country <- aggregate_countries(deaths)

# Function to enlongate data frames
enlongate <- function(data_frame, id, key, value) {
    return(data_frame %>%
        pivot_longer(-id, names_to = key, values_to = value))
}

# Elongate data frames
cases_long <- enlongate(cases_by_country, "ISO 3166-1 Alpha 3-Codes", "date", "cases")
recoveries_long <- enlongate(recoveries_by_country, "Country/Region", "date", "recoveries")
deaths_long <- enlongate(deaths_by_country, "Country/Region", "date", "deaths")

# Join data frames
data_joined <- cases_long %>%
    inner_join(recoveries_long, by = c("Country/Region" = "Country/Region", "date" = "date")) %>%
    inner_join(deaths_long, by = c("Country/Region" = "Country/Region", "date" = "date")) %>%
    mutate(date = as.Date(date, "%m/%d/%y"),
        active_cases = cases - recoveries - deaths)

# Quick plot to validate data
ggplot(data_joined %>% filter(date < as.Date("2020-12-13"), `Country/Region` == "US"),
    aes(x = date, y = active_cases, color = `Country/Region`, group = `Country/Region`)) +
  geom_line() +
  theme(legend.position = "none")

# ---------------------------------------------------------------------------------------
# Most of the above is shit as they discontinued to report recoveries 
# TODO deaths are still tracked but not sure how to extract them
# Switching back to same tactic as in Germany: Assumption people recovery on average after 14d

cases_long_shifted <- cases_long %>%
    mutate(date = as.Date(date, "%m/%d/%y"),
            date_shifted = date - 15) # Subtracting 15 days to get the recovery date [t, t+14]

est_int_active_cases <- cases_long_shifted  %>%
    left_join(cases_long_shifted %>% select(-date_shifted), suffix = c(".orig", ".shifted"),
                by = c("ISO 3166-1 Alpha 3-Codes" = "ISO 3166-1 Alpha 3-Codes", "date_shifted" = "date"),
                keep = TRUE) %>%
    mutate(cases.shifted = replace(cases.shifted, is.na(cases.shifted), 0),
        active_cases = cases.orig - cases.shifted) %>%
    select(country_code = `ISO 3166-1 Alpha 3-Codes.orig`, date = date.orig, active_cases, cases.orig, cases.shifted)


ggplot(est_int_active_cases,
    aes(x = date, y = active_cases, color = country_code, group = country_code)) +
  geom_line() +
  theme(legend.position = "none")

# ---------------------------------------------------------------------------------------
# Normalize data by population

# Load population data
population_data <- read_csv("global_pop.csv") %>%
    select(country = `Country Name`, country_code = `Country Code`, population = `2020`) %>%
    mutate(population = ifelse(country_code == "ERI", 3213969, population))



# Joining cases and population data
est_int_active_cases_pop <- est_int_active_cases %>%
    left_join(population_data, by = c("country_code" = "country_code")) %>%
    mutate(active_cases_norm = active_cases / population) %>%
    select(date, country = country, active_cases, population, active_cases_norm)

# Load movement data and extract unique countries
movement_data <- read_csv("../movement/int_movement_prob.csv") %>%
    pivot_longer(-start, names_to = "dest", values_to = "prob") %>%
    select(dest) %>%
    unique(.)

# Join movement countries and case data to remove countries with no movement data
# TODO set active cases to zero if negative 
est_int_active_cases_pop_mov <- movement_data %>%
    left_join(est_int_active_cases_pop, by = c("dest" = "country")) %>%
    select(-active_cases, -population)

no_data <- movement_data %>%
        left_join(est_int_active_cases_pop, by = c("dest" = "country")) %>%
                    filter(!complete.cases(.))

save(no_data, file = "no_data.Rdata")

# convert to wide format with date as rows to use for approxTime function
int_active_cases <- est_int_active_cases_pop_mov %>%
    filter(complete.cases(.)) %>%
    mutate(active_cases_norm = ifelse(active_cases_norm < 0, NA, active_cases_norm)) %>%
    pivot_wider(date, names_from = "dest", values_from = "active_cases_norm")

save(int_active_cases, file = "int_active_cases.Rdata")


# ---------------------------------------------------------------------------------------

#normalization for suseptibles -> subtract infected from population (very simple approximation)
# TODO need to fix this to include recoverd - ASK IN MEETING 
cases_long <- cases_long %>%
    mutate(date = as.Date(date, "%m/%d/%y"))

    # Joining cases and population data
est_int_susceptibles <- cases_long %>%
    left_join(population_data, by = c("ISO 3166-1 Alpha 3-Codes" = "country_code")) %>%
    mutate(susceptibles = (population - cases) / population) %>%
    select(date, country = country, cases, population, susceptibles)

# TODO if cases where negative this needs to be NA as well
# Join movement countries and susceptibles data to remove countries with no movement data
est_int_susceptibles_mov <- movement_data %>%
    left_join(est_int_susceptibles, by = c("dest" = "country")) %>%
    select(-cases, -population)

# convert to wide format with date as rows to use for approxTime function
int_susceptibles <- est_int_susceptibles_mov %>%
    filter(complete.cases(.)) %>%
    pivot_wider(date, names_from = "dest", values_from = "susceptibles")

ggplot(est_int_susceptibles_mov %>%
    filter(complete.cases(.)),
    aes(x = date, y = susceptibles, color = dest, group = dest)) +
  geom_line() +
  theme(legend.position = "none")

save(int_susceptibles, file = "int_susceptibles.Rdata")
