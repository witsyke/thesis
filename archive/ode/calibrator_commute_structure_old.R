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
setwd("PATH TO SIR EXPERIEMENTS>>")

library(tidyverse)
library(deSolve)
library(reshape2)
source("ode_commute_structure.R")
source("mobility_matrix_extractor_commute.R")

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

#Fill initial state vectors with real values of start date
# TODO filter start_date then go
start_cases <- nat_cases %>%
  filter(date == as.Date(start_date)) %>%
  right_join(national_pop) %>%
  mutate(infected = if_else(is.na(infected), 0, infected),
         recovered = if_else(is.na(recovered), 0, recovered)) %>%
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

#-------------------------------
# DEFINE PARAMETERS
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
beta_start  <- 0.22



#------------------------------------
# ADD MLE CODE DIRECTLY FOR NOW
# Maximum likelihood estimation
library(bbmle)
# likelihood function
sir_ll <- function(lbeta, lalpha_nat, lalpha_int) {
  
  # times - dense timesteps for a more detailed solution
  times <- seq(from = 0, to = days, by = 1)
  parms <- c(beta = plogis(lbeta))
  
  result <- as.data.frame(ode(y = initial_state_values,
                              time = times,
                              func = commute_sir_model,
                              parms = c(parms, params_fixed)))
  
  # select only complete cases, i.e. rows with no NAs, from the dataframe
  dat <- na.omit(nat_cases %>% select(time, j_county, active_case_prop))
  
  # select elements where result time equal dat time and county
  result_long <- melt(result, id = "time")
  temp <- dat %>%
    inner_join(result_long, by = c("time" = "time", "j_county" = "variable"))
  
  # TODO this needs to be adjusted to match the counties rather than the subpops 
  
  # Calculating global prevalence
  # Extract infected proportion
  # j_t <- result %>%
  #   select(starts_with("j_")) %>%
  #   as.matrix(.)
  # 
  # prevalences <- tibble(j = j_t %*% f_n) %>%
  #   rowid_to_column("time") %>%
  #   mutate(time = time - 1)
  
  # For single counties
  std <- sqrt(sum((temp$active_case_prop - temp$value)^2) / length(temp$value))
  
  
  return(-sum(dnorm(temp$active_case_prop, mean = temp$value, sd = std, log = TRUE)))
}
# minimize negative-log-likelihood
fit_mle <- mle2(sir_ll,
                start = list(lbeta = qlogis(beta_start)),
                method = "Nelder-Mead",
                control = list(maxit = 500, trace = 1),
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
  beta = 0.1365, #plogis(coef_test["lbeta"]),
  gamma = 1 / 14, # average recovery rate [d^-1]
  P = P,
  Q = Q,
  kappa = kappa,
  time_mult = 0)

times_test <- seq(from = 0, to = days, by = 1)

result_test2 <- as.data.frame(ode(y = initial_state_values,
                                 time = times_test,
                                 func = commute_sir_model,
                                 parms = params_test,
                                 method = "ode45"))


result_long_test2 <- melt(result_test2, id = "time")



# TODO this is not very efficient and takes quite some time, but its better than nothing and should only matter for very long fittings
results_long_county_test <- Reduce(rbind, lapply(times_test, get_county_values, ode_result = result_long_test2, P = P))

P1 = national_mobility("2020-38", movement_data)
P2 = national_mobility("2020-39", movement_data)
P_new <- national_mobility(row$week, movement_data)
N_ij_new <- N_i * P_new
N_j_new <- colSums(N_ij_new)
Q_new <- t(t(N_ij_new) / N_j_new)


update_sub_populations <- function(current_state, N_t, N_t1){
  print("Updating sub-populations...")
  
  s <- current_state[1:(length(current_state)/2)] # susceptible proportion for the sub-populations (length = 398*398)
  s_matrix = matrix(s, nrow = sqrt(length(s)), byrow = TRUE) # susceptible matrix
  
  
  j <- current_state[(length(current_state)/2+1):length(current_state)] # infected proportion for the sub-populations (length = 398*398)
  j_matrix <- matrix(j, nrow = sqrt(length(j)), byrow = TRUE)
  
  # TODO test if P is easy otherwise use N
  N_diff <- N_t1 - N_t
  # view(N_diff)
  # view(rowSums(N_t1))
  # view(rowSums(N_t))
  # # view(rowSums(P_t1)-rowSums(P_t))
  # view(rowSums(N_diff))
  # test()
  N_diff_neg <- N_diff
  N_diff_neg[N_diff_neg > 0] <- 0
  N_diff_neg <- abs(N_diff_neg)
  
  N_diff_pos <- N_diff
  N_diff_pos[N_diff_pos < 0] <- 0
  
  s_change_neg <- s_matrix * N_diff_neg
  # view(s_change_neg)
  j_change_neg <- j_matrix * N_diff_neg
  # view(j_change_neg)
  
  s_change_pos <- rowSums(s_change_neg) / rowSums(N_diff_neg) * N_diff_pos
  # view(s_change_pos)
  j_change_pos <- rowSums(j_change_neg) / rowSums(N_diff_neg) * N_diff_pos
  
  
  S <- s_matrix * N_t
  I <- j_matrix * N_t
  
  S <- S - s_change_neg + s_change_pos
  I <- I - j_change_neg + j_change_pos
  
  
  S <- S / N_t1
  S[is.nan(S)] <- 0
  S <- as.vector(t(S))
  names(S) <- names(s)
  
  I <- I / N_t1
  I[is.nan(I)] <- 0
  I <- as.vector(t(I))
  names(I) <- names(j)
  
  return(c(S, I))
}

update_subpopulations_simple <- function(current_state, N, P_t, P_t1){
  s <- current_state[1:(length(current_state)/2)] # susceptible proportion for the sub-populations (length = 398*398)
  s_matrix = matrix(s, nrow = sqrt(length(s)), byrow = TRUE) # susceptible matrix
  
  
  j <- current_state[(length(current_state)/2+1):length(current_state)] # infected proportion for the sub-populations (length = 398*398)
  j_matrix <- matrix(j, nrow = sqrt(length(j)), byrow = TRUE)
  
  S_i <- rowSums(s_matrix * (N * P_t))
  I_i <- rowSums(j_matrix * (N * P_t))
  
  s_matrix_new <- S_i * P_t1 # Extract number if susceptible per sub-population
  s_matrix_new <- s_matrix_new / (N * P_t1)
  
  j_matrix_new <- I_i * P_t1 # Extract number if susceptible per sub-population
  j_matrix_new <- j_matrix_new / (N * P_t1)
    
  N_ij_new <- N * P_t1
  N_j_new <- colSums(N_ij_new)
  Q_new <- t(t(N_ij_new) / N_j_new)   
    
}

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


# TODO this can be removed from the loop
times_test <- seq(from = 0, to = 7, by = 1)
output <- 0
final_result <- data.frame()

current_state_values <- initial_state_values
P <- national_mobility(pre_lockdown, movement_data)
N_ij <- N_i * P
N_j <- colSums(N_ij)
Q <- t(t(N_ij) / N_j)

params_test <- list(
  beta = 0.1365, #plogis(coef_test["lbeta"]),
  gamma = 1 / 14, # average recovery rate [d^-1]
  P = P,
  Q = Q,
  kappa = kappa,
  time_mult = 0)

for(i in 1:nrow(movements_filtered)){
  row <- movements_filtered[i,]
  print(row$week)
  
  # TODO something is going really wrong here. The structure update makes the pandemic a lot worse
  # Theoretically the structure update should have minimal impact
  # Seems to be a code problem not a data problem 
  if(row$week_number > 38){
    # view(output)
    print("Updating initial state...")
    # Get subpopulations for next week
    # print(row$week)
    
    P_new <- national_mobility(row$week, movement_data)
    N_ij_new <- N_i * P_new
    N_j_new <- colSums(N_ij_new)
    Q_new <- t(t(N_ij_new) / N_j_new)
    
    
    # Update inital values according to new subpopulations
    # view(N_ij)
    # view(N_ij_new)
    current_state_values <- update_sub_populations(output, N_ij, N_ij_new)
    # current_state_values <- output
    
    # view(P)
    
    P <- P_new
    Q <- Q_new
    N_ij <- N_ij_new
    
    # view(Q)
   
    params_test$P <- P
    params_test$Q <- Q
    params_test$time_mult <- row$mult
    
    # view(params_test$P)
    # view(params_test$Q)
    # view(current_state_values)
  }
  # view(current_state_values)
  print(paste("Running SIR model", row$week, "..."))
  result_test <- as.data.frame(ode(y = current_state_values,
                                   time = times_test,
                                   func = commute_sir_model,
                                   parms = params_test,
                                   method = "ode45"))
  
  print("Updating results...")
  result_long_test <- melt(result_test, id = "time") 
  final_result <- rbind(final_result, result_long_test %>%
                          mutate(time = time + 7 * row$mult))
  # view(row)
  # view(result_long_test)
  # test()
  output_last <- result_long_test %>%
    filter(time == 7)
  # view(output_last)
  output <- output_last$value
  
  names(output) <- output_last$variable
  # view(output)
  # view(output)
}

test <- final_result %>%
  group_by(time, variable) %>%
  summarise(value = mean(value)) %>%
  ungroup()

results_long_county_test_structure <- Reduce(rbind, lapply(unique(test$time), get_county_values, ode_result = test, P = P))

update_sub_populations(c(0.4, 0.3, 0.2, 0.4, 0.3, 0.2, 0.3, 0.3, 0.4, 0.3, 0.3, 0.4),
                       matrix(c(1000, 2000, 1000, 1000, 2000, 1000), ncol = 3, byrow = T),
                       matrix(c(500, 2300, 1200, 500, 2300, 1200), ncol = 3, byrow = T))

update_sub_populations(c(0.2, 0.1, 0.3, 0.7, 0.1, 0.4, 0.4, 0.4, 0.5, 0.2, 0.1, 0.3, 0.7, 0.1, 0.4, 0.4, 0.4, 0.5),
                       matrix(c(0.2, 0.3, 0.5, 0.3, 0.1, 0.6, 0.1, 0.4, 0.5), ncol = 3, byrow = T),
                       matrix(c(100, 200, 400, 100, 400, 300, 200, 500, 700), ncol = 3, byrow = T),
                       matrix(c(140, 210, 350, 240, 80, 480, 140, 560, 700), ncol = 3, byrow = T))


update_sub_populations(c(0.2, 0.1, 0.3, 0.7, 0.1, 0.4, 0.4, 0.4, 0.5, 0.2, 0.1, 0.3, 0.7, 0.1, 0.4, 0.4, 0.4, 0.5),
                       matrix(c(0.1, 0.6, 0.3, 0.4, 0.5, 0.1, 0.3, 0.5, 0.2), ncol = 3, byrow = T),
                       matrix(c(200, 600, 500, 1200, 800, 200, 700, 100, 400), ncol = 3, byrow = T),
                       matrix(c(130, 780, 390, 880, 1100, 220, 360, 600, 240), ncol = 3, byrow = T))

update_sub_populations(c(0, 0.1, 0.3, 0.7, 0.1, 0, 0.4, 0.4, 0.5, 0, 0.9, 0.7, 0.3, 0.9, 0, 0.6, 0.6, 0.5),
                       matrix(c(0.2, 0.3, 0.5, 0, 0.7, 0.3, 0.5, 0.4, 0.1), ncol = 3, byrow = T),
                       matrix(c(0, 200, 500, 300, 400, 0, 700, 800, 200), ncol = 3, byrow = T),
                       matrix(c(140, 210, 350, 0, 490, 210, 850, 680, 170), ncol = 3, byrow = T))

update_sub_populations(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.2, 0.3, 0.4, 0.1, 0.4, 0.2, 0.4, 0.7, 0.3, 0.3, 0.5, 0.1, 0.2, 0.3, 0.4, 0.5, 0.2, 0.3, 0.4, 0.1, 0.4, 0.2, 0.4, 0.7, 0.3, 0.3, 0.5),
                       matrix(c(0.3, 0.2, 0.4, 0.1, 0.4, 0.2, 0.2, 0.2, 0.3, 0.5, 0.1, 0.1, 0.1, 0.3, 0.1, 0.5), ncol = 4, byrow = T),
                       matrix(c(100, 300, 400, 500, 200, 100, 700, 300, 600, 200, 800, 100, 100, 400, 500, 700), ncol = 4, byrow = T),
                       matrix(c(390, 260, 520, 130, 520, 260, 260, 260, 510, 850, 170, 170, 170, 510, 170, 850), ncol = 4, byrow = T))


#----------------- PLOTTING THE RESULT -------------------
ggplot(results_long_county_test_structure, aes(x = time, y = value, color = county, group = county)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16)) +
  labs(title = paste("Test Example simulation with beta",
                     as.character(params_test$beta),
                     "and gamma", as.character(params_test$gamma), 
                     collapse = " ")) +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"))



# TODO need to review this
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

