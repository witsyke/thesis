# Loading necessary libraries
library(deSolve)
library(tidyverse)
library(reshape2)
setwd("<<PATH TO MOVEMENT DATA>>")
# Loading movement matrix
load("flux_frac.Rdata")
load("total_county_fraction.Rdata")

# Get inital infections

cases <- read_csv("../case/cases_lk_week.csv")
county_data <- read_csv("../case/population_per_lk.csv")

# Need to get the scale of the cases back to per-capita by dividing the multiplied 100,000
test <- cases %>%
  select(lk_id, `2020-1`) %>%
  left_join(county_data, by = c("lk_id" = "lk_id")) %>%
  select(lk_id, lk_movement, `2020-1`) %>%
  mutate(cases = `2020-1` / 100000) %>%
  filter(cases != 0,
         complete.cases(.)) 


counties <- nrow(dm_symmetrized_row)

initial_s <- rep(1, counties)
names(initial_s) <- paste("s", rownames(dm_symmetrized_row), sep = "_")
initial_s[paste("s", test$lk_movement, sep = "_")] <- (1)
initial_j <- rep(0, counties)
names(initial_j) <- paste("j", rownames(dm_symmetrized_row), sep = "_")
initial_j[paste("j", test$lk_movement, sep = "_")] <- 0

initial_j["j_Landkreis Zwickau"] = 0.0001
initial_s["s_Landkreis Zwickau"] = 0.9999

# Set start counties to a specific value (need to change s and j where j = 1 - s)

initial_state_values <- c(initial_s, initial_j)
length(initial_state_values)
# beta: infection rate, gamma: recovery rate, alpha: global mobility rate
# alpha was calculated in movement_matrix from Phi/Omega where Phi is the entire mobility flux and Omega is the effective population 0.942596
# gamma: simple assumption: an average a person is infectious for 14 days -> gamma = 1/14
# R0 of COVID-19 as initially estimated by the World Health Organization (WHO) was between 1.4 and 2.4
# using the mean R0 of 1.9 we have beta/gamma = R0 -> beta = R0 * gamma -> beta = 1.9 * 1/14 = 0.1357143
parameters <- list(beta = (2.4 * 1 / 14), gamma = 1 / 14, alpha = 0.942596/20, P = dm_symmetrized_row)
time <- seq(from = 0, to = 365*2, by = 1)

1 - parameters$gamma/parameters$beta 

reduction <- data.frame(times = c(0, 50, 60, 121, 131), import = c(1, 1, 0.4, 0.4, 1))

adjusted_contact_rate <- approxfun(reduction, rule = 2, f = 0, method = "linear")

# Define the ODE system
sir_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    # do a need to extract s and j from state here? 
    s <- state[1:398]
    j <- state[399:796]
    one <- rep(1, length(s))
    
    test <- adjusted_contact_rate(time)
    
    # does it make sense to split this into multiple parts e.g.: the movement
    # ds <- -beta * s * j + alpha * (P %*% s - (P * s) %*% one)
    # dj <- beta * s * j - gamma * j + alpha * (P %*% j - (P * j) %*% one)
    
    ds <- -test * beta * (j/5.275e-06)^10/((j/5.275e-06)^10+1) * s * j + alpha * (P %*% s - (P * s) %*% one)
    dj <- test * beta * (j/5.275e-06)^10/((j/5.275e-06)^10+1) * s * j - gamma * j + alpha * (P %*% j - (P * j) %*% one)
    
    
    return(list(c(ds, dj)))
  })
}

s <- initial_state_values[1:398]
j <- initial_state_values[399:796]
one <- rep(1, length(s))

-parameters$beta * (j/5.275e-06)^50/((j/5.275e-06)^50+1) * s * j + parameters$alpha * (parameters$P %*% s - (parameters$P * s) %*% one)
parameters$beta * (j/5.275e-06)^50/((j/5.275e-06)^50+1) * s * j - parameters$gamma * j + parameters$alpha * (parameters$P %*% j - (parameters$P * j) %*% one)

tes <- parameters$P * j

ptm <- proc.time()
# Solve the ODEs
output <- ode(
  y = initial_state_values,
  times = time,
  func = sir_model,
  parms = parameters
)
proc.time()-ptm
o# Elongating data frame plotting
output_long <- melt(as.data.frame(output), id = "time") %>%
  rename(Compartment = variable)

sd((output_long %>%
  filter(time == 500,
         grepl("s_", Compartment, fixed = TRUE)))$value)

# Calculating global prevalence
j_t <- as.data.frame(output) %>%
  select(starts_with("j_")) %>%
  as.matrix(.)

s_t <- as.data.frame(output) %>%
  select(starts_with("s_")) %>%
  as.matrix(.)  

pit <- as.data.frame(output) %>% select(time, starts_with("j_")) %>% filter(time == 275)

x <- melt(pit, id = "time")$value
x %*% f_n


prev <- tibble(Prevalence = j_t %*% f_n, Sus = s_t %*% f_n) %>%
  rowid_to_column("time") %>%
  mutate(time = time - 1)

max(prev$Prevalence)

# Plotting the data frame
ggplot(output_long, aes(x = time, y = value, color = Compartment, group = Compartment)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none",
        axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size=16)) +
  geom_line(data = prev, aes(x = time, y = Prevalence, color = "black", group = "black"), color="black") +
  geom_line(data = prev, aes(x = time, y = Sus, color = "red", group = "red"), color="black") +
  labs(title = paste("Example simulation with beta", as.character(parameters$beta), "and gamma", as.character(parameters$gamma), "and alpha", as.character(parameters$alpha), collapse = " "))

# Assumption that I'm making right now is that the mobility only has an impact on the time of the peak not the height of the peak
# Height would be to different local situations expressed through additional compartments or different rates
# homogeneity does not holds?
# actually it could -> if the process of the pandemic has not progressed as far a change in parameters would slow the spread where it is -> resulting in a lower peak with the change of a global parameter


ggplot(prev) +
  geom_line(aes(x = time, y = Prevalence, color = "black", group = "black")) +
  geom_line(aes(x = time, y = Sus, color = "red", group = "red")) +
  xlab("Time (days)") +
  ylab("Proportion of people") +
  theme(legend.position = "none")


