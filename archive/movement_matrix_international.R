# Import libraries
library(tidyverse)
library(readr)

# Set working directory 
setwd("<<PATH TO MOVEMENT DATA>>")

# Load movement data and aggregate Berlin
movement_data <- read_csv("complete_movement.csv.gz") %>%
  mutate(kreis1 = replace(kreis1, grepl("Berlin", kreis1, fixed = TRUE), "Berlin"),
         kreis2 = replace(kreis2, grepl("Berlin", kreis2, fixed = TRUE), "Berlin"),
         kreis1 = replace(kreis1, kreis1 == "Eisenach", "Wartburgkreis"),
         kreis2 = replace(kreis2, kreis2 == "Eisenach", "Wartburgkreis")) %>%
  group_by(week, staat1, kreis1, staat2, kreis2) %>%
  summarise(travellers = sum(travellers)) %>%
  ungroup()

# Set all county-county movement to 0 and set start location and end location
movement_int <- movement_data %>%
  mutate(travellers = ifelse(!is.na(kreis1) & !is.na(kreis2), 0, travellers)) %>%
  mutate(kreis1 = ifelse(is.na(kreis1), staat1, kreis1),
        kreis2 = ifelse(is.na(kreis2), staat2, kreis2)) %>%
  filter(kreis1 != "Germany",
         kreis2 != "Germany")
# TODO Problem: there are movements from "Germany" without a county


# Extract unqiue start/ destination locations
unique_locations <- movement_int %>%
  select(loc = kreis1) %>%
  union(movement_int %>%
    select(loc = kreis2)) %>%
  unique()

unique_locations_country <- movement_int %>%
  select(loc = kreis1, state = staat1) %>%
  union(movement_int %>%
    select(loc = kreis2, state = staat2)) %>%
  unique()

# Generate movement for each start-destination-week combination
movement_weeks <- unique_locations %>%
    full_join(unique_locations, by = character(), suffix = c(".start", ".dest")) %>%
    full_join(movement_int %>%
      select(week) %>%
      unique(), by = character())


# Add actual movements to data frame with possible movements
# Add 0 movements
movement_weeks_data <- movement_weeks %>%
    left_join(movement_int, by = c("loc.start" = "kreis1", "loc.dest" = "kreis2", "week")) %>%
    mutate(travellers = ifelse(is.na(travellers), 0, travellers))
    

# Generate matrix of average daily travellers
mean_daily_movements <- movement_weeks_data %>%
  group_by(loc.start, loc.dest) %>%
  summarise(daily_travellers = mean(travellers) / 7)

# Convert data frame to wide format
daily_matrix_wide <- mean_daily_movements %>%
  pivot_wider(names_from = loc.dest, values_from = daily_travellers)


movement_matrix <- data.matrix(daily_matrix_wide %>%
                    ungroup() %>%
                    select(-loc.start))

movement_matrix_upper <- movement_matrix
movement_matrix_lower <- movement_matrix

# Set unnecesary half of the matrix to NA
# TODO this might no longer be necessary if the other apporoach works
movement_matrix_upper[lower.tri(movement_matrix)] <- NA
movement_matrix_lower[upper.tri(movement_matrix)] <- NA


rownames(movement_matrix) <- daily_matrix_wide$loc.start

movement_matrix_symmetrized <- movement_matrix + t(movement_matrix)

min(movement_matrix_symmetrized)

# Set movements between start and itself to 0
diag(movement_matrix_symmetrized) <- 0

sum(movement_matrix_symmetrized)

mean(movement_matrix_symmetrized) / 2

sd(movement_matrix_symmetrized) / mean(movement_matrix_symmetrized)

#(S13) from Brockmann
phi <- sum(movement_matrix_symmetrized)

# (S12) from brockmann
# Creating the Flux fraction matrix 
# Dividing by the full Phi (sum of all elements) automatically adjusts the weight of Fij & Fji to 1/2 
# Both directions together make up x% of the total travel -> travel in one direction makes up half of this
# Sum of the matrix is 1
flux_fraction_matrix <- (movement_matrix_symmetrized / phi)

# Extracting probabilites that movement comes from a county
f_n <- rowSums(flux_fraction_matrix)

# (f_mn) conditional probability that an individual that left m moved to n
int_movement_prob <- flux_fraction_matrix / f_n

int_movement_prob_final <- as.data.frame(int_movement_prob) %>%
  rownames_to_column(var = "start") %>% # start means the country the probabily is for
  pivot_longer(cols = -start, names_to = "dest", values_to = "prob") %>%
  left_join(unique_locations_country, by = c("start" = "loc")) %>%
  left_join(unique_locations_country, by = c("dest" = "loc")) %>%
  filter(state.x != state.y, # filter movements within Germany
        state.x == "Germany") %>% # remove rows of countries -> only counties in rows and countries in columns
  pivot_wider(id_cols = "start", names_from = "dest", values_from = "prob")

write_csv(int_movement_prob_final, file = "int_movement_prob.csv")

# Convert to matrix
int_movement_prob_matrix <- data.matrix(int_movement_prob_final %>%
                    ungroup() %>%
                    select(-start))

rownames(int_movement_prob_matrix) <- int_movement_prob_final$start

save(int_movement_prob_matrix, file = "int_movement_prob_matrix.Rdata")


# Assuming the total devices as total population (as the relative values should be correct)
# HOW MANY DEVICES DOES NETCHECK HAVE? min, max, mean
# Assuming 1.3 million for now
omega <- 1.3 * 10^6
# Currently gamma is in weeks^-1
# why do we use the full Phi here? Theoretically only Phi/2 is amount of travellers
# -> still way to large for it to have significant impact
# is an average constant travel matrix the best approach here
# or is the diffusion between everything to high to cause significant delays
gamma <- (phi / 2) / omega


# -------------------------------------------------------------------------------
# Check if the matrix is symmetric

# Only upper and lower matrix
# Add start destination to matrix and convert to long version - filter locations where start and dest are the same
upper_triangle <- cbind(daily_matrix_wide$loc.start, as_tibble(movement_matrix_upper)) %>%
  rename(start = "daily_matrix_wide$loc.start") %>%
  pivot_longer(-start, names_to = "destination", values_to = "mean_daily_travellers") %>%
  filter(complete.cases(.),
         start != destination)
lower_triangle <- cbind(daily_matrix_wide$loc.start, as_tibble(movement_matrix_lower)) %>%
  rename(start = "daily_matrix_wide$loc.start") %>%
  pivot_longer(-start, names_to = "destination", values_to = "mean_daily_travellers") %>%
  filter(complete.cases(.),
         start != destination)

# Join upper and lower triangle of matrix where ut-start is the same as lt-dest and ut-dest = lt-start
# to get association between F_ij and F_ji
combined <- upper_triangle %>%
  inner_join(lower_triangle, by = c("start" = "destination", "destination" = "start"), keep = T)

# Calculate R^2 by taking the squared correlation between upper and lower triangle
cor(combined$mean_daily_travellers.x, combined$mean_daily_travellers.y)^2

# TODO if these values are the same i dont have to do that nasty stuff from before
# check symmetry of full matrix to its transpose
test_matrix <- mean_daily_movements %>%
  inner_join(mean_daily_movements, by = c("loc.start" = "loc.dest", "loc.dest" = "loc.start")) %>%
  filter(loc.start != loc.dest)

  # TODO was unterscheidet test_matrix und combined?

mean_x <- mean(test_matrix$daily_travellers.x)
ss_res <- sum((test_matrix$daily_travellers.x - test_matrix$daily_travellers.y)^2)
ss_tot <- sum((test_matrix$daily_travellers.x - mean_x)^2)

r_sqr_ <- 1 - ss_res / ss_tot
  
r_sqr <- cor(test_matrix$daily_travellers.x, test_matrix$daily_travellers.y)^2
