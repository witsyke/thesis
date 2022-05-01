# Provide methods to extract mobility matrices based on an start and end date

matrix_r_sqr <- function(lower_triangle, upper_triangle, rownames){
  # Add start destination to matrix and convert to long version - filter locations where start and dest are the same
  upper_triangle <- cbind(rownames, as_tibble(upper_triangle)) %>%
    rename(start = rownames) %>%
    pivot_longer(-start, names_to = "destination", values_to = "mean_daily_travellers") %>%
    filter(complete.cases(.),
           start != destination)
  lower_triangle <- cbind(rownames, as_tibble(lower_triangle)) %>%
    rename(start = rownames) %>%
    pivot_longer(-start, names_to = "destination", values_to = "mean_daily_travellers") %>%
    filter(complete.cases(.),
           start != destination)
  
  # Join upper and lower triangle of matrix where ut-start is the same as lt-dest and ut-dest = lt-start
  # to get association between F_ij and F_ji
  combined <- upper_triangle %>%
    inner_join(lower_triangle, by = c("start" = "destination", "destination" = "start"), keep = T)

  mean_x <- mean(combined$mean_daily_travellers.x)
  ss_res <- sum((combined$mean_daily_travellers.x - combined$mean_daily_travellers.y)^2)
  ss_tot <- sum((combined$mean_daily_travellers.x - mean_x)^2)
  
  return(1 - ss_res / ss_tot)
}


extract_movement_probability <- function(mean_daily_movements){
  mean_daily_movements_wide <- mean_daily_movements %>%
    pivot_wider(names_from = loc.dest, values_from = daily_travellers)
  
  
  daily_movment_matrix <- data.matrix(mean_daily_movements_wide %>%
                                        select(-loc.start))
  
  rownames(daily_movment_matrix) <- mean_daily_movements_wide$loc.start
  
  daily_movment_matrix_upper <- daily_movment_matrix
  daily_movment_matrix_lower <- daily_movment_matrix
  
  # Extract upper and lower matrices from original matrix
  daily_movment_matrix_upper[lower.tri(daily_movment_matrix)] <- NA
  daily_movment_matrix_lower[upper.tri(daily_movment_matrix)] <- NA
  
  # 3. Verify symmetry (other function?)
  print(paste("R squared of the mean mobility matrix: ",
              matrix_r_sqr(daily_movment_matrix_lower, daily_movment_matrix_upper, mean_daily_movements_wide$loc.start)))
  # TODO calculate mean edge weight and coefficient of variation 
  
  
  daily_movement_matrix_symmetrized <- daily_movment_matrix + t(daily_movment_matrix)
  
  # Set movements between start and itself to 0
  diag(daily_movement_matrix_symmetrized) <- 0
  
  
  # 4. Build matrix and flux_fractions (other function? - should be same for both cases)
  #(S13) from Brockmann
  phi <- sum(daily_movement_matrix_symmetrized)
  
  # (S12) from brockmann
  # Creating the Flux fraction matrix 
  # Dividing by the full Phi (sum of all elements) automatically adjusts the weight of Fij & Fji to 1/2 
  # Both directions together make up x% of the total travel -> travel in one direction makes up half of this
  # Sum of the matrix is 1
  flux_fraction_matrix <- (daily_movement_matrix_symmetrized / phi)
  
  # Extracting probabilites that movement comes from a county
  f_n <- rowSums(flux_fraction_matrix)
  
  # (f_mn) conditional probability that an individual that left m moved to n
  movement_prob_matrix <- flux_fraction_matrix / f_n
  #TODO for international movement this needs to be further modified (extra function)
  
  # Assuming the total devices as total population (as the relative values should be correct)
  # HOW MANY DEVICES DOES NETCHECK HAVE? min, max, mean
  # Assuming 1.3 million for now
  omega <- 1.3 * 10^6
  # why do we use the full Phi here? Theoretically only Phi/2 is amount of travellers
  # -> still way to large for it to have significant impact
  # is an average constant travel matrix the best approach here
  # or is the diffusion between everything to high to cause significant delays
  alpha <- (phi / 2) / omega # this is gamma from brockmann
  
  return(list(movement_prob_matrix = movement_prob_matrix, f_n = f_n, alpha = alpha))
}





national_mobility <- function(start_date, end_date, movements) {
  require(tidyverse)
  
  # 1. Filter dates
  start_week <- paste(year = strftime(start_date, format = "%Y"), 
                      week = strftime(start_date, format = "%V"),
                      sep = "-")
  end_week <- paste(year = strftime(end_date, format = "%Y"), 
                      week = strftime(end_date, format = "%V"),
                      sep = "-")
  
  # 1.1. Building full empty matrix for movements for all counties to each other
  movement_weeks <- movements %>%
    filter(complete.cases(.)) %>%
    select(kreis1) %>%
    unique() %>%
    full_join(movements %>%
                filter(complete.cases(.)) %>%
                select(kreis2) %>%
                unique(), by=character()) %>%
    full_join(movements %>% select(week) %>%  
                filter(week >= start_week, 
                       week <= end_week) %>% 
                unique(), by = character())
  
  movements_filtered <- movements %>%
    filter(week >= start_week,
           week <= end_week)
  print(paste("Total number of movements from", start_week, "to", end_week, ":", nrow(movements_filtered)))
  
  # 2. Extract in country movements 
  in_country_movements <- movements_filtered %>%
    filter(complete.cases(.)) %>%
    select(week, kreis1, kreis2, travellers)
  
  # Add actual movements to data frame with possible movements
  # Add 0 movements
  # calculate mean daily travellers
  # TODO should check threshold for this as well
  mean_daily_movements <- movement_weeks %>%
    left_join(in_country_movements, by = c("kreis1", "kreis2", "week")) %>%
    mutate(travellers = ifelse(is.na(travellers), 0, travellers)) %>%
    group_by(kreis1, kreis2) %>%
    summarise(daily_travellers = mean(travellers) / 7) %>%
    rename(loc.start = kreis1, loc.dest = kreis2) %>%
    ungroup()
  #------------------------------
  
  result <- extract_movement_probability(mean_daily_movements)
  
  path <- paste("results/", 
                start_date, 
                end_date, 
                "national_mobility.Rdata", 
                sep = "_")
  save(result, file = path)
  
  return(result)
}


international_mobility <- function(start_date, end_date, movements){
  # Set all county-county movement to 0 and set start location and end location
  movements <- movements %>%
    mutate(travellers = ifelse(!is.na(kreis1) & !is.na(kreis2), 0, travellers)) %>%
    mutate(kreis1 = ifelse(is.na(kreis1), staat1, kreis1),
           kreis2 = ifelse(is.na(kreis2), staat2, kreis2)) %>%
    filter(kreis1 != "Germany",
           kreis2 != "Germany")
  # TODO Problem: there are movements from "Germany" without a county
  
  # 1. Filter dates
  start_week <- paste(year = strftime(start_date, format = "%Y"), 
                      week = strftime(start_date, format = "%V"),
                      sep = "-")
  end_week <- paste(year = strftime(end_date, format = "%Y"), 
                    week = strftime(end_date, format = "%V"),
                    sep = "-")
  
  # 1.1. Generate possible movements
  # Extract unqiue start/ destination locations
  unique_locations <- movements %>%
    select(loc = kreis1) %>%
    union(movements %>%
            select(loc = kreis2)) %>%
    unique()
  
  unique_locations_country <- movements %>%
    select(loc = kreis1, state = staat1) %>%
    union(movements %>%
            select(loc = kreis2, state = staat2)) %>%
    unique()
  
  # Generate movement for each start-destination-week combination
  movement_weeks <- unique_locations %>%
    full_join(unique_locations, by = character(), suffix = c(".start", ".dest")) %>%
    full_join(movements %>%
                select(week) %>%
                filter(week >= start_week,
                       week <= end_week) %>%
                unique(), by = character())
  
  
  movements_filtered <- movements %>%
    filter(week >= start_week,
           week <= end_week)
  print(paste("Total number of movements from", start_week, "to", end_week, ":", nrow(movements_filtered)))
  
  
  # Add actual movements to data frame with possible movements
  # Add 0 movements
  # calculate mean daily travellers
  mean_daily_movements <- movement_weeks %>%
    left_join(movements_filtered, by = c("loc.start" = "kreis1", "loc.dest" = "kreis2", "week")) %>%
    mutate(travellers = ifelse(is.na(travellers), 0, travellers)) %>%
    group_by(loc.start, loc.dest) %>%
    summarise(daily_travellers = mean(travellers) / 7) %>%
    ungroup()
  
  result <- extract_movement_probability(mean_daily_movements)
  
  # TODO this is not optimal but should do for now
  load("data/no_data.Rdata")  # countries with no case data
  
  movement_prob <- as.data.frame(result$movement_prob_matrix) %>%
    rownames_to_column(var = "start") %>% # start means the country the probabily is for
    pivot_longer(cols = -start, names_to = "dest", values_to = "prob") %>%
    left_join(unique_locations_country, by = c("start" = "loc")) %>%
    left_join(unique_locations_country, by = c("dest" = "loc")) %>%
    filter(state.x != state.y, # filter movements within Germany
           state.x == "Germany") %>% # remove rows of countries -> only counties in rows and countries in columns
    pivot_wider(id_cols = "start", names_from = "dest", values_from = "prob") %>%
    select(!no_data$dest)
  
  # Convert to matrix
  movement_prob_matrix_final <- data.matrix(movement_prob %>%
                                            ungroup() %>%
                                            select(-start))
  
  rownames(movement_prob_matrix_final) <- movement_prob$start
  
  result$movement_prob_matrix <- movement_prob_matrix_final
  
  
  path <- paste("results/", 
                start_date, 
                end_date, 
                "international_mobility.Rdata", 
                sep = "_")
  save(result, file = path)
  
  return(result)
  
}

