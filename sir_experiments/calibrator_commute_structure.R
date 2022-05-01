#-------------------------------- FUNCTIONS --------------------------------
# Function to extract the county values for a specific point in time
get_county_values <- function(filter_time, ode_result, P){
  j <- ode_result %>%
    filter(time == filter_time,
           grepl("j_", variable, fixed = T))
  
  j_matrix <- matrix(j$value, nrow = sqrt(length(j$value)), byrow = T)
  rownames(j_matrix) <- rownames(P)
  colnames(j_matrix) <- colnames(P)
  return(tibble(time = filter_time, county = rownames(P), value = rowSums(P * j_matrix)))
}



#-------------------------------- LOAD MOVEMENT DATA --------------------------------
# This needs to be set to the directory where the current file is located
setwd("<<PATH TO CALIBRATOR SCRIPT>>")

library(tidyverse)
library(deSolve)
library(reshape2)
source("ode_commute_structure.R")
source("mobility_matrix_extractor_commute.R")
source("update_subpopulations.R")

# TODO need to update this with dates for the second wave
start_date <- "2020-09-14"
end_date <- "2021-01-04"
pre_lockdown <- "2020-38"


days = as.double(as.Date(end_date) - as.Date(start_date))



#-------------------------------- LOAD MOVEMENT DATA --------------------------------
# This will take a little while and needs a bit of RAM if this doesn't work, let me know
# then I can extract a smaller movement dataset, e.g., only 2020
movement_data <- read_csv("data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()


movements_filtered <- movement_data %>%
  separate(week, c("year", "week_number"), sep = "-", remove = F) %>%
  mutate(year = as.numeric(year),
         week_number = as.numeric(week_number)) %>%
  filter(year == 2020,
         week_number >= 38,
         week_number <= 53) %>%
  select(week, year, week_number) %>%
  distinct() %>%
  rownames_to_column() %>%
  mutate(mult = as.numeric(rowname) - 1)


P <- national_mobility(pre_lockdown, movement_data)
P2 <- national_mobility("2020-", movement_data)


#---------------------------------- LOAD TRAINING DATA ----------------------------------

national_cases <- read_csv("data/est_norm_national_data.csv")
national_pop <- read_csv("data/national_pop_data.csv") %>%
  mutate(n_county = paste("n", lk, sep = "_"),)

dates_to_index <- as_tibble_col(seq(as.Date(start_date), as.Date(end_date), by = "days"), column_name = "date") %>%
  rownames_to_column("time") %>%
  mutate(time = as.numeric(time) - 1)

# TODO needs to have active cases and susceptible 
# for this i need to somehow get the recovered population at that point in time
nat_cases <- dates_to_index %>%
  left_join(national_cases, by = c("date" = "date"))

comparision_cases <- nat_cases %>%
  mutate(prevalence = infected / population) %>%
  right_join(national_pop) %>%
  select(time, county = lk, prevalence)

#Fill initial state vectors with real values of start date
# TODO filter start_date then go
start_cases <- nat_cases %>%
  filter(date == as.Date(start_date)) %>%
  right_join(national_pop) %>%
  mutate(infected = if_else(is.na(infected), 0, infected),
         recovered = if_else(is.na(recovered), 0, recovered)) %>%
  mutate(j_county = paste("j", lk, sep = "_"),
         s_county = paste("s", lk, sep = "_"))

#   
#--------------------------------- DEFINE INTIAL VALUES -----------------------
# Generation of initial value vectors for Germany
# Extract all county names
num_counties <- nrow(P)
sub_population_names <- tibble(rowname = rownames(P)) %>%
  full_join(tibble(colname = colnames(P)), by=character()) %>%
  mutate(sub_population = paste(rowname, colname, sep = "&"))

N_i <- rep(1, num_counties)
names(N_i) <- paste("n", rownames(P), sep = "_")
N_i[national_pop$n_county] <- national_pop$population

N_ij <- N_i * P
N_j <- colSums(N_ij)
Q <- t(t(N_ij) / N_j)

# Generate vector for suseptible proportions at t_0 -> all are one as Germany had no cases
initial_s <- rep(1, num_counties)
names(initial_s) <- paste("s", rownames(P), sep = "_")
initial_s[start_cases$s_county] <- start_cases$population - (start_cases$infected + start_cases$recovered) 

# Generate vector for intected proportions at t_0 -> all are one as Germany had no cases
initial_j <- rep(0, num_counties)
names(initial_j) <- paste("j", rownames(P), sep = "_")
initial_j[start_cases$j_county] <- start_cases$infected


inital_s_sub <- initial_s * P # Extract number if susceptible per sub-population
inital_s_sub <- inital_s_sub / N_ij # get proportion of susceptible per sub-pop + convert to vector
inital_s_sub[is.nan(inital_s_sub)] <- 0 # Set to very high number to validate if there are any propblems <- this number should always be irrelevant as P and Q are 0 at this position <- 0 is important if I do structural updates and add people to this compartment 
inital_s_sub <- as.vector(t(inital_s_sub)) #
names(inital_s_sub) <- paste("s", sub_population_names$sub_population, sep = "_")

inital_j_sub <- initial_j * P
inital_j_sub <- inital_j_sub / N_ij # get proportion of susceptible per sub-pop + convert to vector
inital_j_sub[is.nan(inital_j_sub)] <- 0 # Set to very high number to validate if there are any propblems <- this number should always be irrelevant as P and Q are 0 at this position <- 0 is important if I do structural updates and add people to this compartment 

inital_j_sub <- as.vector(t(inital_j_sub)) #
names(inital_j_sub) <- paste("j", sub_population_names$sub_population, sep = "_")

# Combine initial states for s and j to single vector
# TODO these currently don't have have names. FIX THIS
initial_state_values <- c(inital_s_sub, inital_j_sub)

#------------------------------- DEFINE PARAMETERS ------------------------------------
# kappa is the reduction in the contact rate as a linear factor of the reduction in movement
ones <- rep(1, num_counties)
kappa_fixed <-  matrix(
  # time, kappa by county, 
  c(  0, ones,
      50, ones,
      100, ones), ncol=num_counties + 1, byrow=TRUE
) 

load("data/kappas_baseline_week38.Rdata")
kappa <- kappas %>%
  select(time, kappa, county) %>%
  pivot_wider(time, names_from = county, values_from = kappa)

# Define additional parameters that are not part of the optimization
params_fixed <- list(
  gamma = 1 / 14, # average recovery rate [d^-1]
  P = P, # county-county matrix movement probabilities
  Q = Q,
  kappa = kappa)

# values to start optimisation
beta_start  <- 0.1365
gamma_start <- 1 / 14

#------------------------------------
# Maximum likelihood estimation
library(bbmle)
# likelihood function
sir_ll_commute_structure <- function(lbeta, lgamma) {
  
  result <- run_experiment(list(beta = plogis(lbeta), gamma = plogis(lgamma)),
                           initial_state_values = initial_state_values, 
                           pre_lockdown = pre_lockdown, 
                           movement_data = movement_data, 
                           N_i = N_i, 
                           weeks = movements_filtered, 
                           movement_reduction = TRUE, 
                           structural_update = TRUE)
  
  
  # select elements where result time equal dat time and county
  test <- result$result %>%
    filter(!(time %in% (movements_filtered$mult * 7))) %>%
    union(result$result %>%
            filter(time %in% (movements_filtered$mult * 7)) %>%
            group_by(time, variable) %>%
            summarise(value = mean(value)) %>%
            ungroup())
  
  results_long_county_test_structure <- Reduce(rbind, lapply(unique(test$time), get_county_values, ode_result = test, P = result$P))
  
  temp <- comparision_cases %>%
    inner_join(results_long_county_test_structure, by = c("time" = "time", "county" = "county"))
  

  # For single counties
  std <- sqrt(sum((temp$prevalence - temp$value)^2) / length(temp$value))
  
  
  return(-sum(dnorm(temp$prevalence, mean = temp$value, sd = std, log = TRUE)))
}
# minimize negative-log-likelihood

fit_mle <- mle2(sir_ll_commute_structure,
                start = list(lbeta = qlogis(beta_start), lgamma = qlogis(gamma_start)),
                method = "Nelder-Mead",
                control = list(trace = 1),
                trace = TRUE)

# Save results
save(fit_mle, file = paste("results_calibration/", 
                      start_date, 
                      end_date, 
                      "mle_fit_final.Rdata", 
                      sep = "_"))

summary(fit_mle)

# Calculate profile and save
p1 <- profile(fit_mle)
coef_test <- coef(fit_mle)

save(p1, file = paste("results_calibration/", 
                      start_date, 
                      end_date, 
                      "profile_final.Rdata", 
                      sep = "_"))
