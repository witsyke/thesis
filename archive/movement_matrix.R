library(tidyverse)
library(readr)

setwd("<<PATH TO MOVEMENT DIRECTORY>>")

data <- read_csv("complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

ptm <- proc.time()
# Building full matrix
movement_weeks <- data %>%
    filter(complete.cases(.)) %>%
    select(kreis1) %>%
    unique() %>%
    full_join(data %>%
    filter(complete.cases(.)) %>%
    select(kreis2) %>%
    unique(), by=character()) %>%
    full_join(data %>% select(week) %>% unique(), by = character())
proc.time() - ptm

# General preprocessing
data_no_foreign <- data %>%
    filter(complete.cases(.)) %>%
    select(week, kreis1, kreis2, travellers)

asdf <- data %>%
  filter(complete.cases(.)) %>%
  select(week, kreis1, kreis2, devices1, devices2, travellers) %>%
  filter(kreis2 == "Alb-Donau-Kreis", week == "2020-01")

sum(asdf$travellers)

# Idea: use the number devices that were recorded instead of the number of devices that they should have
# Theoretically the number of devices shouldn't change (leave -1, enter +1) however with their way of aggregation
# this doesn't seem to be the case. We have ~2*10^6 devices on average but they don't record more than ~1.5*10^6 devices
# in the end, as long as the number of movements makes sense
# might need to aggregate to Bundesland -> commuting might have a to strong effect on the global mobility rate


lol <- asdf %>% 
  group_by(week, kreis2) %>%
  summarise(trav = sum(travellers),
    dev = sum(devices2),
    travellers_prop = sum(travellers) / sum(devices2),
    n = n(),
    x = sum(devices2) / n(),
    y = sum(travellers) / (sum(devices2) / n()))

asdf %>% 
  group_by(week, kreis1) %>%
  summarise(trav = sum(travellers) / 7,
            dev = sum(devices1),
            travellers_prop = sum(travellers) / sum(devices2),
            n = n(),
            x = sum(devices1) / n(),
            y = sum(travellers) / (sum(devices1) / n()))

o <- lol %>%
  filter(week == "2020-01")

sum(o$travellers_prop)

# Remove movements from same county to same county

full_matrix <- movement_weeks %>%
    left_join(data_no_foreign, by = c("kreis1", "kreis2", "week"))

# Add 0 movements and Remove movements from same county to same county
full_matrix <- full_matrix %>%
  mutate(travellers = ifelse(is.na(travellers), 0, travellers))
# %>%
#  filter(kreis1 != kreis2)
  
weekly_matrix <- full_matrix %>%
  group_by(kreis1, kreis2) %>%
  summarise(mean_travellers = mean(travellers),
                count_weeks = n(),
                sum_travellers = sum(travellers),
            daily_travellers = mean(travellers) / 7)



weekly_matrix_wide <- weekly_matrix %>%
  select(-count_weeks, -sum_travellers, -daily_travellers) %>%
  pivot_wider(names_from = kreis2, values_from = mean_travellers)

day_matrix_wide <- weekly_matrix %>%
  select(-count_weeks, -sum_travellers, -mean_travellers) %>%
  pivot_wider(names_from = kreis2, values_from = daily_travellers)

rownames(weekly_matrix_wide) <- weekly_matrix_wide$kreis1

# weekly_matrix_wide <- weekly_matrix_wide %>%
#   ungroup() %>%
#   select(-kreis1)

wm <- data.matrix(weekly_matrix_wide %>%
                    ungroup() %>%
                    select(-kreis1), rownames.force = T)
dm <- data.matrix(day_matrix_wide %>%
                    ungroup() %>%
                    select(-kreis1), rownames.force = T)

wm_upper <- wm
wm_lower <- wm
dm_upper <- dm
dm_lower <- dm

wm_lower[lower.tri(wm)] <- NA
wm_lower[upper.tri(wm)] <- NA

dm_lower[lower.tri(dm)] <- NA
dm_lower[upper.tri(dm)] <- NA

rownames(wm) <- weekly_matrix_wide$kreis1

wm_symmetrized <- wm + t(wm)

rownames(dm) <-day_matrix_wide$kreis1

# Talk to katharina about the sum of passengers and how 
dm_symmetrized <- dm + t(dm)

min(dm_symmetrized)

# tx <- cor(wm_lower, t(wm_upper))

diag(wm_symmetrized) <- 0
diag(dm_symmetrized) <- 0

sum(dm_symmetrized)
sum(wm_symmetrized)

mean(dm_symmetrized) / 2

sd(dm_symmetrized) / mean(dm_symmetrized)

#(S13) from Brockmann
Phi <- sum(wm_symmetrized)
Phi_d <- sum(dm_symmetrized)

# (S12) from brockmann
# Creating the Flux fraction matrix 
# Dividing by the full Phi (sum of all elements) automatically adjusts the weight of Fij & Fji to 1/2 
# Both directions together make up x% of the total travel -> travel in one direction makes up half of this
# Sum of the matrix is 1 
wm_symmetrized_flux_frac <- (wm_symmetrized / Phi)
dm_symmetrized_flux_frac <- (dm_symmetrized / Phi_d)

# Extracting probabilites that movement comes from a county
# f_n <- rowSums(wm_symmetrized_flux_frac)
f_n <- rowSums(dm_symmetrized_flux_frac)

save(f_n, file = "total_county_fraction.Rdata")

# (f_mn) conditional probability that an individual that left m moved to n
wm_symmetrized_row <- wm_symmetrized_flux_frac / f_n
dm_symmetrized_row <- dm_symmetrized_flux_frac / f_n_d

dim(dm_symmetrized_row)

save(dm_symmetrized_row, file = "flux_frac.Rdata")

# From the paper, as wm_symmetrized_flux_frac is symmetric (only for show, would need to be transposed again for formula)
wm_symmetrized_col <- t(wm_symmetrized_flux_frac / f_n)

rowSums(wm_symmetrized_row)
colSums(wm_symmetrized_col)

# Assuming the total devices as total population (as the relative values should be correct)
# HOW MANY DEVICES DOES NETCHECK HAVE? min, max, mean
# Assuming 1.3 million for now
Omega <- 1.3 * 10^6
# Currently gamma is in weeks^-1
gamma <- Phi / Omega
# why do we use the full Phi here? Theoretically only Phi/2 is amount of travellers
# -> still way to large for it to have significant impact
# is an average constant travel matrix the best approach here
# or is the diffusion between everything to high to cause significant delays
gamma_d <- Phi_d / Omega
# gamma in days^-1
gamma <- gamma / 7

# Testing if relations are the same when using the absolute movement values
test <- rowSums(wm_symmetrized)
wm_symmetrized_row_test <- wm_symmetrized / test

# Add start destination to matrix and convert to long version - filter locations where start and dest are the same
upper_triangle <- cbind(weekly_matrix_wide$kreis1, as_tibble(wm_upper)) %>%
  rename(start = "weekly_matrix_wide$kreis1") %>%
  pivot_longer(-start, names_to = "destination", values_to = "mean_weekly_travellers") %>%
  filter(complete.cases(.),
         start != destination)
lower_triangle <- cbind(weekly_matrix_wide$kreis1, as_tibble(wm_lower)) %>%
  rename(start = "weekly_matrix_wide$kreis1") %>%
  pivot_longer(-start, names_to = "destination", values_to = "mean_weekly_travellers") %>%
  filter(complete.cases(.), 
         start != destination)

# Join upper and lower triangle of matrix where ut-start is the same as lt-dest and ut-dest = lt-start
# to get association between F_ij and F_ji
combined <- upper_triangle %>%
  inner_join(lower_triangle, by = c("start" = "destination", "destination" = "start"), keep = T) %>%
  mutate(sym = mean_weekly_travellers.x + tst3$mean_weekly_travellers.y)

sum(combined$sym)

cor(combined$mean_weekly_travellers.x, combined$mean_weekly_travellers.y)^2

# check symmetry
test_matrix <- weekly_matrix %>%
  inner_join(weekly_matrix, by = c("kreis1" = "kreis2", "kreis2" = "kreis1"))

mean_x <- mean(test_matrix$mean_travellers.x)
SS_res <- sum((test_matrix$mean_travellers.x - test_matrix$mean_travellers.y)^2)
SS_tot <- sum((test_matrix$mean_travellers.x - mean_x)^2)

Rsqr_ <- 1 - SS_res/SS_tot
  
Rsqr <- cor(test_matrix$mean_travellers.x, test_matrix$mean_travellers.y)^2


o <- c(1, 2, 4.5, 7)
mean(o) * length(o)
sum(o)


