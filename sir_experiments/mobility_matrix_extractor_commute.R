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


 extract_movement_proportions <- function(mean_daily_movements){
  mean_daily_movements_wide <- mean_daily_movements %>%
    pivot_wider(names_from = loc.dest, values_from = daily_travellers)
  
  
  F_ij <- data.matrix(mean_daily_movements_wide %>%
                                        select(-loc.start))
  
  rownames(F_ij) <- mean_daily_movements_wide$loc.start
  
  # TODO calculate mean edge weight and coefficient of variation 
  
  
  # Extracting probabilites that movement comes from a county
  F_i <- rowSums(F_ij)
  
  P = F_ij / F_i
  
  return(P)
}


national_mobility <- function(extract_week, movements, trace = FALSE) {
  require(tidyverse)

  
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
                filter(week == extract_week) %>% 
                unique(), by = character())
  
  movements_filtered <- movements %>%
    filter(week == extract_week)
  if(trace){
    print(paste("Total number of movements in", extract_week, "to", ":", nrow(movements_filtered)))
  }
  
  # 2. Extract in country movements 
  in_country_movements <- movements_filtered %>%
    filter(complete.cases(.)) %>%
    select(week, kreis1, kreis2, travellers)
  
  # Add actual movements to data frame with possible movements
  # Add 0 movements
  # calculate mean daily travellers
  # TODO sthis could be simplyfied as im only using 1 week now
  mean_daily_movements <- movement_weeks %>%
    left_join(in_country_movements, by = c("kreis1", "kreis2", "week")) %>%
    mutate(travellers = ifelse(is.na(travellers), 0, travellers)) %>%
    group_by(kreis1, kreis2) %>%
    summarise(daily_travellers = mean(travellers) / 7) %>%
    rename(loc.start = kreis1, loc.dest = kreis2) %>%
    ungroup()
  #------------------------------
  
  result <- extract_movement_proportions(mean_daily_movements)
  
  # path <- paste("results/", 
  #               start_date, 
  #               end_date, 
  #               "national_mobility.Rdata", 
  #               sep = "_")
  # save(result, file = path)
  
  return(result)
}
