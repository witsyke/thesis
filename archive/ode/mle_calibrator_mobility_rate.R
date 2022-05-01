#-------------------------------- LOAD MOVEMENT DATA --------------------------------
# This needs to be set to the directory where the current file is located
setwd("<<PATH TO SIR EXPERIEMENTS>>")

library(tidyverse)
library(deSolve)
library(reshape2)
source("ode_mobility_rate.R")
source("mobility_matrix_extractor.R")

start_date = "2020-09-10"
end_date = "2020-10-26"

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

nat_mobility <- national_mobility(start_date, end_date, movement_data)
# int_mobility <- international_mobility(start_date, end_date, movement_data)


#---------------------------------- LOAD INTERNATIONAL CASE DATA --------------------------------

# load("data/int_active_cases.Rdata") # active_cases
# # TODO CHECK IF I CONSIDER RECOVERED HERE - I DON'T - ASK IN MEETING 
# load("data/int_susceptibles.Rdata") # int_susceptibles

#---------------------------------- LOAD TRAINING DATA ----------------------------------

national_cases <- read_csv("data/est_norm_national_case_data.csv")

dates_to_index <- as_tibble_col(seq(as.Date(start_date), as.Date(end_date), by = "days"), column_name = "date") %>%
  rownames_to_column("time") %>%
  mutate(time = as.numeric(time) - 1)

# TODO needs to have active cases and susceptible 
# for this i need to somehow get the recovered population at that point in time
nat_cases <- dates_to_index %>%
  left_join(national_cases, by = c("date" = "date")) %>%
  mutate(j_county = paste("j", lk, sep = "_"),
         s_county = paste("s", lk, sep = "_"))


#--------------------------------- PLOT COUNTY CURVES  -----------------------
ggplot(nat_cases %>% filter(lk == "Hamburg"), aes(x = time, y = active_case_prop, color = lk)) +
  geom_line() +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16))
  
# 
# test <- rownames(nat_mobility$movement_prob_matrix)
# 
# as_tibble(test) %>%
#   left_join(nat_cases %>% filter(date == as.Date(end_date)), by = c("value" = "lk")) %>%
#   filter(is.na(time))
#   
#--------------------------------- DEFINE INTIAL VALUES -----------------------

# Generation of initial value vectors for Germany
# Extract all county names
num_counties <- nrow(nat_mobility$movement_prob_matrix)

# Generate vector for suseptible proportions at t_0 -> all are one as Germany had no cases
initial_s <- rep(1, num_counties)
names(initial_s) <- paste("s", rownames(nat_mobility$movement_prob_matrix), sep = "_")
# Generate vector for intected proportions at t_0 -> all are one as Germany had no cases
initial_j <- rep(0, num_counties)
names(initial_j) <- paste("j", rownames(nat_mobility$movement_prob_matrix), sep = "_")

#Fill initial state vectors with real values of start date
# TODO filter start_date then go
start_cases <- nat_cases %>%
  filter(date == as.Date(start_date))


initial_s[start_cases$s_county] <- 1 - start_cases$active_case_prop # TODO this needs to be replaced with the actual number
initial_j[start_cases$j_county] <- start_cases$active_case_prop

# Combine initial states for s and j to single vector
initial_state_values <- c(initial_s, initial_j)


# s_int_values <- dates_to_index %>%
#   left_join(int_susceptibles) %>%
#   select(-date)
#   
# j_int_values <- dates_to_index %>%
#   left_join(int_active_cases) %>%
#   select(-date)

#-------------------------------
# DEFINE PARAMETERS

# Define additional parameters that are not part of the optimization
params_fixed <- list(
  gamma = 1 / 14, # average recovery rate [d^-1]
  P = nat_mobility$movement_prob_matrix, # county-county matrix movement probabilities
  # Q = int_mobility$movement_prob_matrix,
  # s_int_values = s_int_values,
  # j_int_values = j_int_values,
  epsilon = 5.275e-06)

# values to start optimisation
beta_start  <- 0.45
alpha_nat_start <- rep(nat_mobility$alpha, num_counties)
names(alpha_nat_start) <- paste("lalpha", rownames(nat_mobility$movement_prob_matrix), sep = "_")
# alpha_int_start <- int_mobility$alpha


#-----------------------------------

# Define the function that approximates the reduction of contacts during a time
# This can for example be used to simulate lockdowns
# contact_reduction <- data.frame(times = c(0, 55, 65, 70, 105, 110, 365), import = c(1, 1, 0.3, 0.2, 0.3, 1, 1))
contact_reduction <- data.frame(times = c(0, 365), import = c(1, 1))
contact_rate_adjstmnt <- approxfun(contact_reduction, rule = 2, f = 0, method = "linear")
# Define the function that approximates the reduction of the amount of in-country travel
# This can for example be used to simulate lockdowns
nat_mov_reduction <- data.frame(times = c(0, 365), import = c(1, 1))
nat_mob_rate_adjstmnt <- approxfun(nat_mov_reduction, rule = 2, f = 0, method = "linear")

# Define the function that approximates the reduction of the amount of out-of-country travel
# This can for example be used to simulate lockdowns
# int_mov_reduction <- data.frame(times = c(0, 365), import = c(1, 1))
# int_mob_rate_adjstmnt <- approxfun(int_mov_reduction, rule = 2, f = 0, method = "linear")


#------------------------------------
# ADD MLE CODE DIRECTLY FOR NOW
# Maximum likelihood estimation
library(bbmle)
# likelihood function
sir_ll <- function(params) {
  # times - dense timesteps for a more detailed solution
  times <- seq(from = 0, to = days, by = 1)
  
  parms <- list(beta = plogis(unname(params[1])), alpha_nat = plogis(unname(params[2:length(params)])))
  print(parms$beta)
  print(parms$alpha_nat[1])
  # print("IT")
  
  result <- as.data.frame(ode(y = initial_state_values,
                              time = times,
                              func = sir_model,
                              parms = c(parms, params_fixed)))
  
  # select only complete cases, i.e. rows with no NAs, from the dataframe
  dat <- na.omit(nat_cases %>% select(time, j_county, active_case_prop))
  
  # select elements where result time equal dat time and county
  result_long <- melt(result, id = "time")
  temp <- dat %>%
    inner_join(result_long, by = c("time" = "time", "j_county" = "variable"))
  
  # TODO @Katharina when you are looking at this already the following maximum likelihood calculation
  # is something I'm very concious about: temp$active_case_prop is the observed prevalence by county
  # temp$value is what the model estimates per county and length(temp$value) is the total number of
  # comparisons (counties * timesteps)

  # For single counties
  std <- sqrt(sum((temp$active_case_prop - temp$value)^2) / length(temp$value))
  
  
  return(-sum(dnorm(temp$active_case_prop, mean = temp$value, sd = std, log = TRUE)))
}

parnames(sir_ll) <- c("lbeta", names(alpha_nat_start))

sink("output.txt")
# minimize negative-log-likelihood
fit_mle <- mle2(sir_ll,
              start = c(lbeta = qlogis(beta_start), qlogis(alpha_nat_start)),
              method = "Nelder-Mead",
              control = list(maxit = 500, trace = 2),
              vecpar = TRUE,
              trace = TRUE)

save(fit_mle, file = paste("results/", 
                    start_date, 
                    end_date, 
                    "mle_fit.Rdata", 
                    sep = "_"))

summary(fit_mle)

coef_test <- coef(fit_mle)


# TODO code to join real data with output for a longer period of time

#---------------------------------------
# TEST FITTED VALUES
params_test <- list(
  beta = plogis(coef_test["lbeta"]),
  alpha_nat = plogis(coef_test["lalpha_nat"]),
  alpha_int = plogis(coef_test["lalpha_int"]),
  gamma = 1 / 14, # average recovery rate [d^-1]  §
  
  P = nat_mobility$movement_prob_matrix, # county-county matrix movement probabilities
  Q = int_mobility$movement_prob_matrix,
  s_int_values = s_int_values,
  j_int_values = j_int_values,
  epsilon = 5.275e-06)

times_test <- seq(from = 0, to = days+30, by = 1)

result_test <- as.data.frame(ode(y = initial_state_values,
                            time = times_test,
                            func = sir_model,
                            parms = params_test))

result_long_test <- melt(result_test, id = "time")

#----------------- PLOTTING THE RESULT -------------------
ggplot(result_long_test %>% filter(grepl("j_", variable, fixed = TRUE)), aes(x = time, y = value, color = variable, group = variable)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16)) +
  labs(title = paste("Test Example simulation with beta",
                     as.character(params_test$beta),
                     "and gamma", as.character(params_test$gamma),
                     "and alpha_nat", as.character(params_test$alpha_nat),
                     "and alpha_int", as.character(params_test$alpha_int), collapse = " "))


#---------------- CALCULATE R2 FOR ALL COUNTIES ----------------------
dates_to_index_test <- as_tibble_col(seq(as.Date(start_date), as.Date(end_date)+30, by = "days"), column_name = "date") %>%
  rownames_to_column("time") %>%
  mutate(time = as.numeric(time) - 1)

nat_cases_test <- dates_to_index_test %>%
  left_join(national_cases, by = c("date" = "date")) %>%
  mutate(j_county = paste("j", lk, sep = "_"),
         s_county = paste("s", lk, sep = "_"))

comparison_test <- nat_cases_test %>%
  inner_join(result_long_test, by = c("time" = "time", "j_county" = "variable")) %>%
  select(time, date, lk, observed = active_case_prop, modelled = value)

save(comparison_test, file = paste("results/", 
                           start_date, 
                           end_date, 
                           "comparison.Rdata", 
                           sep = "_"))

comparison_fitted <- comparison_test %>%
  filter(time <= days)

mean_x <- mean(comparison_fitted$observed)
ss_res <- sum((comparison_fitted$observed - comparison_fitted$modelled)^2)
ss_tot <- sum((comparison_fitted$observed - mean_x)^2)

r_sqr <- 1 - ss_res / ss_tot


test <- comparison_fitted %>%
  group_by(lk) %>%
  summarise(mean_x = mean(observed),
            ss_res = sum((observed - modelled)^2),
            ss_tot = sum((observed - mean_x)^2)) %>%
  mutate(r_sqr = (1 - ss_res / ss_tot))

median(test$r_sqr)


#---------------- PLOT A SPECIFIC COUNTY - MODELLED VS OBSERVED ---------------
comparison_long_test <- comparison_test %>%
  pivot_longer(c("observed", "modelled"), names_to = "type", values_to = "value")

# Select county to compare
comparison_long_test_county <- comparison_long_test %>%
  filter(lk == "Stuttgart")

ggplot() +
  geom_line(data = comparison_long_test_county, aes(x = time, y = value, color = type, group = type)) +
  geom_vline(xintercept = days) +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16)) +
  labs(title = paste(unique(comparison_long_test_county$lk),
                     " - Test Example simulation with beta",
                     as.character(params_test$beta),
                     "and gamma", as.character(params_test$gamma),
                     "and alpha_nat", as.character(params_test$alpha_nat),
                     "and alpha_int", as.character(params_test$alpha_int), collapse = " "))

