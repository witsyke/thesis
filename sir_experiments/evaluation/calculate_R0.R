# ---------------- LIBRARIES ----------------
library(tidyverse)

# SET PATH TO THIS DIRECTORY 
setwd("<<PATH TO THIS DIRECTORY>>")
source("../mobility_matrix_extractor_commute.R")


# ---------------- FUNCTIONS ----------------
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

# ---------------- DATA LOADING ----------------
load("../data/kappas_baseline_week38.Rdata")
load("../results_calibration/_2020-09-14_2021-01-04_mle_fit_final.Rdata")

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


# ----------------  PARAMETERS ----------------

gamma_min <- 0.05469196
gamma_max <- 0.05823306
beta_min <- 0.10623319
beta_max <- 0.11074408

beta_fit <- plogis(coef(fit_mle)["lbeta"])
gamma_fit <- plogis(coef(fit_mle)["lgamma"])


# ----------------  EXPERIMENT SETTINGS ----------------
output_path <- "<<PATH TO LARGE STORAGE>>"
# Chose scneario to run here by updating experiment 
experiment <- "extended_no_partial"
structural_update <- TRUE 
base_week <- "2020-38"
upper_limit <- 53
beta_type <- "fit"
gamma_type <- "fit"


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

# -----
if(beta_type == "fit"){
  beta <- beta_fit
}
if(beta_type == "min"){
  beta <- beta_min
}
if(beta_type == "max"){
  beta <- beta_max
}
# -----
if(gamma_type == "fit"){
  gamma <- gamma_fit
}
if(gamma_type == "min"){
  gamma <- gamma_min
}
if(gamma_type == "max"){
  gamma <- gamma_max
}



# UPDATE BASIC PARAMETERS
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
if(experiment== "no_mobility"){
  structural_update <- FALSE
}

# LOAD KAPPA AND ADJUST DEPENDING ON SCENARIO
kappa_final <- kappas %>%
  select(time, kappa, county) %>%
  pivot_wider(time, names_from = county, values_from = kappa)

if(experiment == "no_mobility"){
  ones <- rep(1, num_counties)
  kappa_final <-  matrix(
    # time, kappa by county, 
    c(  0, ones,
        50, ones,
        100, ones), ncol=num_counties + 1, byrow=TRUE
  ) 
}

if(experiment %in% c("partial_from_beginning", "only_partial")){
  kappa_final <- kappas %>%
    select(time, kappa, county) %>%
    filter(time <= 7*9) %>%
    pivot_wider(time, names_from = county, values_from = kappa)
}

if(experiment == "extended_at_partial"){
  kappa_final <- kappas %>%
    select(time, kappa, county) %>%
    pivot_wider(time, names_from = county, values_from = kappa) %>%
    filter(time <= 7 * 7 | time >= 14 * 7) %>%
    mutate(time = if_else(time == 98, 7 * 8, time),
           time = if_else(time == 105, 8 * 8, time))
}

if(experiment == "extended_no_partial"){
  kappa_final <-  kappas %>%
    select(time, kappa, county) %>%
    pivot_wider(time, names_from = county, values_from = kappa) %>%
    filter(time == 0) %>%
    mutate(time = 0 * 7) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 1 * 7)) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 2 * 7)) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 3 * 7)) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 4 * 7)) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 5 * 7)) %>%
    union(
      kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time == 0) %>%
        mutate(time = 6 * 7)) %>% 
    union(kappas %>%
            select(time, kappa, county) %>%
            pivot_wider(time, names_from = county, values_from = kappa) %>%
            filter(time > 0) %>%
            mutate(time = time + 6 * 7)) %>%
    filter(time <= 7 * 13 | time >= 140) %>%
    mutate(time = if_else(time == 140, 6 * 7 + 7 * 8, time),
           time = if_else(time == 147, 6 * 7 + 8 * 8, time))
}


# ADJUST DAY DATA FRAME DEPENDING ON SCENARIO
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

week_mult <- tibble(weekday=0:6)
times <- movements_filtered %>%
  full_join(week_mult, by = character()) %>%
  mutate(time = weekday + 7 * mult,
         time_update = weekday + 7 * update_mult) %>%
  select(week, year, week_number, time, time_update)


# USET UP INTIAL SUBPOPULATIONS
P <- national_mobility(base_week, movement_data)

# get N_ij for week 47 and 53
national_pop <- read_csv("../data/national_pop_data.csv") %>%
  mutate(n_county = paste("n", lk, sep = "_"),)

# Extract all county names
num_counties <- nrow(P)
sub_population_names <- tibble(rowname = rownames(P)) %>%
  full_join(tibble(colname = colnames(P)), by=character()) %>%
  mutate(sub_population = paste(rowname, colname, sep = "&"))

N_i <- rep(1, num_counties)
names(N_i) <- paste("n", rownames(P), sep = "_")
N_i[national_pop$n_county] <- national_pop$population

N_ij <- N_i * P

# RESET R0 DATA FRAME
# R0_over_time <- data.frame(R0_county=integer(),
#            time=integer()) 
R0_over_time <- data.frame(start=character(),
                           dest=character(),
                           R0_ij_weighted=integer(),
                           R0_ij=integer(),
                           pop=integer(),
                           time=integer()) 


# ----------------  CALCULATE R0 time series ----------------
for(i in 1:nrow(times)){
  row <- times[i,]
  print(row$week)
  print(row$time)
  if(row$year == 2020 & row$week_number >= 38 & row$week_number <= upper_limit & structural_update){
    print("Updating N_ij")
    P <- national_mobility(row$week, movement_data)
    num_counties <- nrow(P)
    sub_population_names <- tibble(rowname = rownames(P)) %>%
      full_join(tibble(colname = colnames(P)), by=character()) %>%
      mutate(sub_population = paste(rowname, colname, sep = "&"))
    
    N_i <- rep(1, num_counties)
    names(N_i) <- paste("n", rownames(P), sep = "_")
    N_i[national_pop$n_county] <- national_pop$population
    
    N_ij <- N_i * P
  }

  
  kappa_t <- as.numeric(approxTime(kappa_final, row$time_update, rule = 2, method = "linear")[-1])
  print(kappa_t)
  
  names(kappa_t) <- rownames(P)
  
  kappa_t <- as.data.frame(kappa_t) %>% 
    rownames_to_column()
  colnames(kappa_t) <- c("county", "kappa")
  
  # print(kappa_t)
  
  betas_local <- kappa_t %>%
    mutate(beta_local = kappa * beta,
           R0_local = beta_local/gamma) %>%
    select(county, kappa, beta_local, R0_local)
  
  # join 2x with N_ij to get the suppopulations 1x for week 47 and one time for week 53
  R0_t <- as.data.frame(N_ij) %>%
    rownames_to_column() %>%
    pivot_longer(-c(rowname), names_to = "dest", values_to = "pop") %>%
    select(start = rowname, dest, pop) %>%
    left_join(betas_local, by = c("start" = "county")) %>%
    left_join(betas_local, by = c("dest" = "county")) %>%
    select(start, dest, pop, beta_local.x, beta_local.y, R0_local.x, R0_local.y) %>%
    mutate(R0_ij = (R0_local.x + R0_local.y) / 2,
           R0_ij_weighted = R0_ij * pop) %>%
    select(start, dest, R0_ij, R0_ij_weighted, pop) %>%
    mutate(time = row$time)
  
  
  R0_over_time <- rbind(R0_over_time, R0_t)
  
}


write_csv(R0_over_time, file = paste(output_path, experiment, beta_type, gamma_type, "2.csv", sep = "_"))

R0_over_time_plot <- R0_over_time %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) 

lockdown = tibble(type = c("Partial lockdown", "Lockdown"), time = c(7*7, 13*7))

ggplot(R0_over_time_plot, aes(x=time, y=R0_country, group=1)) +
  geom_line() + 
  geom_vline(data = lockdown, aes(xintercept=time, linetype=type)) + 
  geom_hline(yintercept=1, color="red") + 
  ylab("Basic reproduction number") +
  xlab("Time t") +
  scale_y_continuous(limits = c(0, 2.25)) +
  theme_bw() +
  theme(axis.title=element_text(size=22,face="bold"),
        axis.text = element_text(size=20),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        legend.position = c(0.95, 0.95))


# -------------- R0 plot with all experiment runs -----------------
beta_type <- "fit"
gamma_type <-  "fit"

observed <- read_csv(paste(output_path, "observed", 
                           beta_type, gamma_type, "2.csv", sep = "_"))  %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Observed mobility",
         experiment_id = "0")

no_lockdown <- read_csv(paste(output_path, "no_mobility", 
                              beta_type, gamma_type, "2.csv", sep = "")) %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "No mobility change",
         experiment_id = "1")

partial_lockdown_only <- read_csv(paste(output_path, "only_partial", 
                                         beta_type, gamma_type, "2.csv", sep = "_"))  %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Only partial lockdown mobility",
         experiment_id = "2")

partial_lockdown_start <- read_csv(paste(output_path, "partial_from_beginning", 
                                         beta_type, gamma_type, "2.csv", sep = "_")) %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Partial lockdown mobility from the beginning",
         experiment_id = "3") 

full_at_partial_lockdown <- read_csv(paste(output_path, "extended_at_partial", 
                                           beta_type, gamma_type, "2.csv", sep = "_")) %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Extended lockdown mobility at the partial lockdown",
         experiment_id = "4") 


full_lockdown_only <- read_csv(paste(output_path, "extended_no_partial", 
                                     beta_type, gamma_type, "2.csv", sep = "_"))  %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Only extended lockdown mobility",
         experiment_id = "5")

full_lockdown_start <- read_csv(paste(output_path, "extended_from_beginning", 
                                      beta_type, gamma_type, "2.csv", sep = "_"))  %>%
  group_by(time) %>%
  summarise(R0_country = sum(R0_ij_weighted) / sum(pop)) %>%
  mutate(experiment = "Extended lockdown mobility from the beginning",
         experiment_id = "6")


combined <- rbind(observed, 
                  no_lockdown, 
                  full_lockdown_start, 
                  partial_lockdown_start,
                  partial_lockdown_only,
                  full_lockdown_only,
                  full_at_partial_lockdown) %>%
  mutate(experiment = factor(experiment, 
                             levels = c("Observed mobility", 
                                        "No mobility change", 
                                        "Only partial lockdown mobility", 
                                        "Partial lockdown mobility from the beginning",
                                        "Extended lockdown mobility at the partial lockdown",
                                        "Only extended lockdown mobility",
                                        "Extended lockdown mobility from the beginning")))
  

write_csv(combined, file = paste(output_path, "/R0_combined2.csv", sep = ""))

lockdown = tibble(type = c("Partial lockdown", "Lockdown"), time = c(7*7, 13*7))


ggplot(combined, aes(x = time, y = R0_country, color = experiment)) +
  geom_vline(data = lockdown, aes(xintercept=time, linetype=type), size = 1) + 
  geom_hline(yintercept=1, color="red", linetype="longdash", size = 1) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Time t (in days)") +
  ylab("Basic reproduction number") +
  scale_y_continuous(limits = c(0, 2.25)) +
  scale_colour_manual(values = c("#00C094", "#F8766D", "#00B6EB", "#53B400", "#A58AFF", "#C49A00", "#FB61D7")) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1, nrow = 4)) +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        # legend.key.size = unit(2, "line"),
        axis.title = element_text(size = 22, face="bold"),
        axis.text = element_text(size=20))


# -------- COUNTY-WISE R0 -----------
observed_all <- read_csv(paste(output_path, "observed", 
                       beta_type, gamma_type, ".csv", sep = "_"))

R0_county <- observed_all %>%
  group_by(time, start) %>%
  summarise(R0_county = sum(R0_ij_weighted) / sum(pop))

view(R0_county %>%
  filter(time == (53-38)*7))

