# SCRIPT TO RUN SIR SIMULATIONS GIVEN A SCENARIO AND A SET OF PARAMETERS
# Requires mobility data that is not publicly available.
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
setwd("<<PATH TO THIS EVALUATION SCRIPT>>")

library(tidyverse)
library(deSolve)
library(reshape2)
library(bbmle)
source("ode_commute_structure.R")
source("mobility_matrix_extractor_commute.R")
source("update_subpopulations.R")
source("sir_experiment.R")

start_date <- "2020-09-14"
end_date <- "2021-01-04"


# THIS IS WHERE THE EXPERIMENT IS SET
experiment <- "observed"

mobility_reduction <- TRUE
structural_update <- TRUE 
pre_lockdown <- "2020-38"
upper_limit <- 53

if(experiment == "partial_from_beginning"){
  structural_update <- FALSE
  pre_lockdown <- "2020-47"
  upper_limit <- 47
}
if(experiment == "only_partial"){
  upper_limit <- 47
}
if(experiment == "extended_from_beginning"){
  structural_update <- FALSE
  pre_lockdown <- "2020-53"
}
if(experiment == "no_mobility"){
  structural_update <- FALSE
  mobility_reduction <- FALSE
}





days = as.double(as.Date(end_date) - as.Date(start_date))



#-------------------------------- LOAD MOVEMENT DATA --------------------------------
# Mobility data is not public
movement_data <- read_csv("data/complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()


movement_data_temp <- movement_data %>%
  separate(week, c("year", "week_number"), sep = "-", remove = F) %>%
  mutate(year = as.numeric(year),
         week_number = as.numeric(week_number)) 
  
  
movements_filtered <- movement_data_temp %>% 
  filter(year == 2020,
         week_number >= 38,
         week_number <= 53) %>%
  union(movement_data_temp %>%
          filter(year == 2021,
                 week_number >= 1,
                 week_number <= 52)) %>%
  select(week, year, week_number) %>%
  distinct() %>%
  rownames_to_column() %>%
  mutate(mult = as.numeric(rowname) - 1,
         update_mult = mult)


if(experiment == "partial_from_beginning"){
  movements_filtered <- movements_filtered %>%
    mutate(update_mult = 9)
}
if(experiment == "extended_from_beginning"){
  movements_filtered <- movements_filtered %>%
    mutate(update_mult = 15)
}
if(experiment == "extended_at_partial"){
  movements_filtered <- movements_filtered %>%
    mutate(week_number = if_else(week_number == 46, 52, if_else(week_number == 47,  53, if_else(week_number >= 48,  54, week_number))),
           week = if_else(week == "2020-46", "2020-52", week),
           week = if_else(week == "2020-47", "2020-53", week))
}
if(experiment == "extended_no_partial"){
  movements_filtered <- movements_filtered %>%
    mutate(week_number = if_else(week_number < 45 & year == 2020, 38, if_else(week_number < 52 & year == 2020, week_number - 6, week_number)),
           week = paste(year, week_number, sep = "-"))
}


P <- national_mobility(pre_lockdown, movement_data)

# view(national_mobility("2020-38", movement_data))

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
# load fitted parameters
# CAREFUL - CURRENTLY THE PARAMETERS FOR THE CHOSEN PARAMETRIZATION HAVE TO SET MANUALLY 
load("results_calibration/_2020-09-14_2021-01-04_mle_fit_final.Rdata")
gamma_min <- 0.05469196
gamma_max <- 0.05823306
beta_min <- 0.10623319
beta_max <- 0.11074408

beta_fit <- plogis(coef(fit_mle)["lbeta"])
gamma_fit <- plogis(coef(fit_mle)["lgamma"])


kappa <- rbind(kappas %>%
  select(time, kappa, county) %>%
  pivot_wider(time, names_from = county, values_from = kappa) %>%
  filter(time == 105) %>%
  mutate(time = 0),
  kappas %>%
    select(time, kappa, county) %>%
    pivot_wider(time, names_from = county, values_from = kappa) %>%
    filter(time == 105) %>%
    mutate(time = 1))

params_test <- list(
  beta = beta, 
  gamma = gamma,
  P = P,
  Q = Q,
  kappa = kappa,
  time_mult = 0)

times_test <- seq(from = 0, to = days + 200, by = 1)

result_test2 <- as.data.frame(ode(y = initial_state_values,
                                  time = times_test,
                                  func = commute_sir_model,
                                  parms = params_test,
                                  method = "ode45"))


result_long_test2 <- melt(result_test2, id = "time")



results_long_county_test <- Reduce(rbind, lapply(times_test, get_county_values, ode_result = result_long_test2, P = P))


#------------------------------- EXPERIMENT ------------------------------------

# running experiment with fitted parameters
# PARAMETRIZATION HAS TO BE SET MANUALLY HERE
result <- run_experiment(list(beta = beta_fit, gamma = gamma_fit),
                         initial_state_values = initial_state_values, 
                         pre_lockdown = pre_lockdown, 
                         movement_data = movement_data, 
                         N_i = N_i, 
                         weeks = movements_filtered, 
                         upper_limit = upper_limit,
                         experiment = experiment,
                         movement_reduction = mobility_reduction, 
                         structural_update = structural_update, 
                         trace = F)
# ~ 3min of runtime



result_combined <- result$result %>%
  filter(!(time %in% (movements_filtered$mult * 7))) %>%
  union(result$result %>%
          filter(time %in% (movements_filtered$mult * 7)) %>%
          group_by(time, variable) %>%
          summarise(value = mean(value)) %>%
          ungroup())

results_long_county_test_structure <- Reduce(rbind, lapply(unique(result_combined$time), get_county_values, ode_result = result_combined, P = result$P))

# SAVE RESULTS TO CSV - careful - currently the set of parameters for parametrization has to be changed manually
write_csv(results_long_county_test_structure, file = paste("results_epidemic_curve/experiment", experiment, "fit_long.csv", sep = "_"))

# Save complete output for all subcompartment (very large ~8GB)
# write_csv(result_combined, file = paste("<<PATH TO LARGE FILE STORAGE>>", experiment, "fit_long_all.csv", sep = "_"))

#----------------- PLOTTING THE RESULT -------------------
ggplot(results_long_county_test_structure, aes(x = time, y = value, color = county, group = county)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16))
