# Loading necessary libraries
library(deSolve)
library(tidyverse)
library(reshape2)


# defining approx function for matrix
approxTime <- function(x, xout, ...) {
  if (is.data.frame(x)) {
    x <- as.matrix(x); wasdf <- TRUE
  } else wasdf <- FALSE
  if (!is.matrix(x)) stop("x must be a matrix or data frame")
  m <- ncol(x)
  y <- matrix(0, nrow = length(xout), ncol = m)
  y[, 1] <- xout
  for (i in 2:m) {
    y[, i] <- as.vector(approx(x[, 1], x[, i], xout, ...)$y)
  }
  if (wasdf) y <- as.data.frame(y)
  names(y) <- dimnames(x)[[2]]
  y
}


setwd("<<PATH TO MOVEMENT DATA>>")

# Loading national movement data
# National movment probability matrix
load("flux_frac.Rdata") # P
# Total travel flux fraction per county
load("total_county_fraction.Rdata") # f_n

# Loading international movement data
# International movment probability matrix
load("int_movement_prob_matrix.Rdata") # Q


# Load active case date for international counties
load("../case/int_active_cases.Rdata") # active_cases
load("../case/no_data.Rdata") # countries with no case data
load("../case/int_susceptibles.Rdata") # int_susceptibles

# Remove countries with no data from movement
# TODO load data from file
int_movement_prob_final <- read_csv("int_movement_prob.csv")
int_movement_prob_complete <- int_movement_prob_final %>%
  select(!no_data$dest)

int_movement_prob_matrix <- data.matrix(int_movement_prob_complete %>%
                    ungroup() %>%
                    select(-start))

rownames(int_movement_prob_matrix) <- int_movement_prob_complete$start

# Generation of inital value vectors for Germany
# Extract all county names
counties <- nrow(dm_symmetrized_row)

# Generate vector for suseptible proportions at t_0 -> all are one as Germany had no cases
initial_s <- rep(1, counties)
names(initial_s) <- paste("s", rownames(dm_symmetrized_row), sep = "_")

# Generate vector for intected proportions at t_0 -> all are one as Germany had no cases
initial_j <- rep(0, counties)
names(initial_j) <- paste("j", rownames(dm_symmetrized_row), sep = "_")

# Combine initial states for s and j to single vector
initial_state_values <- c(initial_s, initial_j)


test <- read_csv("../case/est_case_data_norm.csv") %>%
  select(date, lk_movement, active_case_prop) %>%
  left_join(
    as_tibble_col(seq(as.Date("2020-01-22"), as.Date("2021-11-21"), by = "days")) %>%
      rownames_to_column("time") %>%
      mutate(time = as.numeric(time) - 2),
    by = c("date" = "value")) %>%
  mutate(j_county = paste("j", lk_movement, sep = "_"),
        s_county = paste("s", lk_movement, sep = "_")) %>%
  select(time, j_county, s_county, active_case_prop) %>%
  filter(time == 0)

initial_j[test$j_county] <- test$active_case_prop


initial_s[test$s_county] <- 1 - test$active_case_prop
initial_state_values <- c(initial_s, initial_j)
# Definition of parameters for the ODE


s_int_values <- int_susceptibles %>%
  rownames_to_column("time") %>%
  mutate(time = as.numeric(time) - 2) %>%
  filter(time >= 0) %>%
  select(-date)
j_int_values <- int_active_cases %>%
  rownames_to_column("time") %>%
  mutate(time = as.numeric(time) - 2) %>%
  filter(time >= 0) %>%
  select(-date)
parameters <- list(
  beta = 0.2184057, # , # 0.8049305 * 0.5685, # average number of secondary infections per infected case [d^-1]
  gamma = 1 / 14, # average recovery rate [d^-1]
  alpha_nat = 0.004287166, # 0.004287166, # , # in-country mobility rate
  P = dm_symmetrized_row, # county-county matrix movement probabilities
  alpha_int = 0.0174649, # out of country mobility rate
  Q = int_movement_prob_matrix,
  s_int_values = s_int_values,
  j_int_values = j_int_values,
  epsilon = 5.275e-06)

# Definition of the time sequence for which the values should be calculated
time <- seq(from = 0, to = 60, by = 1)

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
int_mov_reduction <- data.frame(times = c(0, 365), import = c(1, 1))
int_mob_rate_adjstmnt <- approxfun(int_mov_reduction, rule = 2, f = 0, method = "linear")

# Define the ODE system
sir_model <- function(time, state, parameters) {
  with(as.list(c(parameters)), {

    s_int <- as.numeric(approxTime(s_int_values, time, rule = 2, method = "linear")[-1])
    j_int <- as.numeric(approxTime(j_int_values, time, rule = 2, method = "linear")[-1])
    one_int <- rep(1, length(s_int))

    s <- state[1:398] # suseptible proportion for the state
    j <- state[399:796] # infected proportion for the state
    one <- rep(1, length(s)) # Vector of 1s for rowSum <- what is better?

    threshold_fun <- (j / epsilon)^10 / ((j / epsilon)^10 + 1) # nolint
    lambda <- contact_rate_adjstmnt(time) * beta * threshold_fun * j

    # TODO could make movement into function
    # Defining the part of the ODE deals with national and international movement
    nat_mov_s <- nat_mob_rate_adjstmnt(time) * alpha_nat * (P %*% s - (P * s) %*% one) # nolint
    int_mov_s <- int_mob_rate_adjstmnt(time) * alpha_int * (Q %*% s_int - (Q * s) %*% one_int) # nolint
    ds <- -lambda * s + nat_mov_s + int_mov_s

    nat_mov_j <- nat_mob_rate_adjstmnt(time) * alpha_nat * (P %*% j - (P * j) %*% one) # nolint
    int_mov_j <- int_mob_rate_adjstmnt(time) * alpha_int * (Q %*% j_int - (Q * j) %*% one_int) # nolint
    dj <- lambda * s  - gamma * j + nat_mov_j + int_mov_j


    return(list(c(ds, dj), s_int, j_int))
  })
}

# Solve the ODE system
output <- ode(
  y = initial_state_values,
  times = time,
  func = sir_model,
  parms = parameters
)

# Elongating data frame plotting
# TODO switch this to tidyverse
output_long <- melt(as.data.frame(output), id = "time") %>%
  rename(Compartment = variable)


results2 <- output_long %>%
  left_join(case_data, by = c("time" = "time", "Compartment" = "county")) %>%
  filter(grepl("j_", Compartment, fixed = TRUE))
save(results2,  file = "results2.Rdata")



# Calculating global prevalence
# Extract infected proportion
j_t <- as.data.frame(output) %>%
  select(starts_with("j_")) %>%
  as.matrix(.)

# Extract susecptilbe proportion
s_t <- as.data.frame(output) %>%
  select(starts_with("s_")) %>%
  as.matrix(.)


prevalences <- tibble(Prevalence = j_t %*% f_n, Sus = s_t %*% f_n) %>%
  rowid_to_column("time") %>%
  mutate(time = time - 1)

# ----------------------------------------------------------------------------------

# Plotting the data frame
ggplot(output_long %>% filter(grepl("j_", Compartment, fixed = TRUE)), aes(x = time, y = value, color = Compartment, group = Compartment)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16)) +
  # geom_line(data = case_data, aes(x = time, y = active_case_prop, color = county, group = county)) +
  # geom_line(data = prevalences, aes(x = time, y = Prevalence, color = "black", group = "black"), color = "black") +
  # geom_line(data = prevalences, aes(x = time, y = Sus, color = "red", group = "red"), color = "black") +
  labs(title = paste("Test Example simulation with beta",
    as.character(parameters$beta),
    "and gamma", as.character(parameters$gamma),
    "and alpha", as.character(parameters$alpha), collapse = " "))

ggplot(prevalences, aes(x = time, y = Prevalence, group = "black"), , color = "black") +
  geom_line() +
  # geom_line(data = prevalences, aes(x = time, y = Prevalence, color = "black", group = "black"), color = "black") +
  geom_line(data = total_cases %>% filter(time <= 60), aes(x = time, y = total_active_cases_prop, group = "black"), color = "blue") +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none")



ggplot(output_long %>% filter(grepl("j_Hof", Compartment, fixed = TRUE)), aes(x = time, y = value, group = Compartment), color = "red") +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16)) +
  geom_line(data = case_data %>% filter(grepl("j_Hof", county, fixed = TRUE)), aes(x = time, y = active_case_prop, group = county), color = "blue" ) +
  # geom_line(data = prevalences, aes(x = time, y = Prevalence, color = "black", group = "black"), color = "black") +
  # geom_line(data = prevalences, aes(x = time, y = Sus, color = "red", group = "red"), color = "black") +
  labs(title = "j_Hof", collapse = " ")

# Assumption that I'm making right now is that the mobility only has an impact
# on the time of the peak not the height of the peak
# Height would be to different local situations expressed through additional compartments or different rates
# homogeneity does not holds?
# actually it could -> if the process of the pandemic has not progressed
# as far a change in parameters would slow the spread where it is -
# > resulting in a lower peak with the change of a global parameter


ggplot(prev) +
  geom_line(aes(x = time, y = Prevalence, color = "black", group = "black")) +
  geom_line(aes(x = time, y = Sus, color = "red", group = "red")) +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none")


# ----------------------------------------------------------------------------------
# Sum of squares calibration
SIR_SSQ <- function(parameters, dat, params_fit) {  # parameters must contain beta & alpha

  # times - dense timesteps for a more detailed solution
  times <- seq(from = 0, to = 60, by = 0.5)

    # calculate model output using your SIR function with ode()

  result <- as.data.frame(ode(y = initial_state_values  # vector of initial state 
                                                          # values, with named elements
                              , times = times             # vector of times
                              , func = sir_model             # your predefined SIR function
                              , parms = c(parameters, params_fit))       # the parameters argument
                                                          # entered with SIR_SSQ()
    )

    # select only complete cases, i.e. rows with no NAs, from the dataframe
  dat <- na.omit(dat)

    # select elements where results$time is in dat$time
  result_long <- melt(result, id = "time")
  temp <- dat %>%
    inner_join(result_long, by = c("time" = "time", "county" = "variable")) %>%
    mutate(delta2 = (value - active_case_prop)^2)

  return(sum(temp$delta2))

}

# values to start optimisation
beta_start  <- 0.45
alpha_start <- 0.49


# Extract county case data to calculate ssq with
case_data <- read_csv("../case/est_case_data_norm.csv") %>%
  select(date, lk_movement, active_case_prop) %>%
  left_join(
    as_tibble_col(seq(as.Date("2020-01-22"), as.Date("2021-11-21"), by = "days")) %>%
      rownames_to_column("time") %>%
      mutate(time = as.numeric(time) - 2),
    by = c("date" = "value")) %>%
  mutate(county = paste("j", lk_movement, sep = "_")) %>%
  select(date, time, county, active_case_prop) %>%
  filter(time >= 0,
        time <= 60)


ggplot(case_data, aes(x = time, y = active_case_prop, color = county)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none")

load("est_active_cases_prop.RData")

total_cases <- est_active_cases_prop %>%
  left_join(
    as_tibble_col(seq(as.Date("2020-01-22"), as.Date("2021-11-21"), by = "days")) %>%
      rownames_to_column("time") %>%
      mutate(time = as.numeric(time) - 2),
      by = c("date" = "value")) %>%
      filter(complete.cases(.),
            time >= 0,
            time <= 100) %>%
      select(-date, -total_active_cases)

ggplot(prevalences, aes(x = time, y = Prevalence, color = "black", group = "black"), , color = "black") +
  geom_line() +
  # geom_line(data = prevalences, aes(x = time, y = Prevalence, color = "black", group = "black"), color = "black") +
  geom_line(data = total_cases, aes(x = time, y = total_active_cases_prop, color = "black", group = "black"), color = "blue") +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none")


# Define additional parameters that are not part of the optimization
parameters_fit <- list(
  gamma = 1 / 14, # average recovery rate [d^-1]
  P = dm_symmetrized_row, # county-county matrix movement probabilities
  alpha_int = 0.0174649, # out of country mobility rate
  Q = int_movement_prob_matrix,
  s_int_values = s_int_values,
  j_int_values = j_int_values,
  epsilon = 5.275e-06)

# Run the optimisation to find the best parameters
ptm <- proc.time()
optimised <- optim(par = c(beta = beta_start, alpha_nat = alpha_start),
                          fn = SIR_SSQ,
                          dat = case_data,
                          params_fit = parameters_fit,
                  method = "BFGS",
                  control = list(trace = 2))
proc.time() - ptm

optimised

# ----------------------------------------------------------------------------------
# Maximum likelihood estimation
library(bbmle)
# likelihood function
sir_ll <- function(lbeta, lalpha_nat) {

  # times - dense timesteps for a more detailed solution
    times <- seq(from = 0, to = 60, by = 1)
    parms <- c(beta = plogis(lbeta), alpha_nat = plogis(lalpha_nat))

    result <- as.data.frame(ode(y = initial_state_values,
              time = times,
              func = sir_model,
              parms = c(parms, parameters_fit)))

  # select only complete cases, i.e. rows with no NAs, from the dataframe
  dat <- na.omit(total_cases)

  # select elements where result time equal dat time and county
  # result_long <- melt(result, id = "time")
  # temp <- dat %>%
  #   inner_join(result_long, by = c("time" = "time", "county" = "variable"))


  # Calculating global prevalence
  # Extract infected proportion
  j_t <- result %>%
    select(starts_with("j_")) %>%
    as.matrix(.)

  prevalences <- tibble(j = j_t %*% f_n) %>%
    rowid_to_column("time") %>%
    mutate(time = time - 1)

  temp <- dat %>%
    inner_join(prevalences, by = c("time" = "time"))
    # Using the length of vector with all datapoints

  # view(temp)

  # For single counties
  # sd <- sqrt(sum((temp$active_cases_prop - temp$value)^2) / length(temp$value))

  sd <- sqrt(sum((temp$total_active_cases_prop - temp$j)^2) / length(temp$j))


  # return(-sum(dnorm(temp$active_cases_prop, mean = temp$value, sd = sd, log = TRUE)))
  return(-sum(dnorm(temp$total_active_cases_prop, mean = temp$j, sd = sd, log = TRUE)))
}
# minimize negative-log-likelihood
ptm <- proc.time()
fit <- mle2(sir_ll,
            start = list(lbeta = qlogis(beta_start), lalpha_nat = qlogis(alpha_start)),
            method = "Nelder-Mead",
            control = list(maxit = 1E5, trace = 1),
            trace = TRUE)
proc.time() - ptm

summary(fit)