# TODO when I adapt this for the change of movement per week I need to divide by 7 to get daily difference
# Should only matter if threshold is applied

extract_total_movement_per_county <- function(start_week, end_week, movement_data, movement = "all", include_local = FALSE){
  # movement can be "in", "out", "all"
  
  movements_filtered <- movement_data %>%
    filter(week >= start_week,
           week <= end_week)
  print(paste("Total number of movements from", start_week, "to", end_week, ":", nrow(movements_filtered)))
  
  # 2. Extract in country movements 
  in_country_movements <- movements_filtered %>%
    filter(complete.cases(.)) %>%
    dplyr::select(week, kreis1, kreis2, travellers)
  
  if(!include_local){
    print("Removing self-loops...")
    in_country_movements <- in_country_movements %>%
      filter(kreis1 != kreis2) # deactivate this to include local movement
  }
  
  
  # TODO this is where I would have to implement the threshold
  
  movements_out <- in_country_movements %>%
    group_by(week, kreis1) %>%
    summarise(sum_out = sum(travellers)) %>%
    ungroup() %>%
    dplyr::select(week, kreis = kreis1, sum = sum_out)
  
  movements_in <- in_country_movements %>%
    group_by(week, kreis2) %>%
    summarise(sum_in = sum(travellers)) %>%
    ungroup() %>%
    dplyr::select(week, kreis = kreis2, sum = sum_in)
  
  if(movement == "all"){
    output <- movements_out %>%
      dplyr::union(movements_in) %>%
      group_by(week, kreis) %>%
      summarise(sum_comp = sum(sum))
  }
  if(movement == "out"){
    output <- movements_out %>%
      mutate(sum_comp = sum)
  }
  if(movement == "in"){
    output <- movements_in %>%
      mutate(sum_comp = sum)
  }
  
  return(output %>%
           group_by(kreis) %>%
           summarise(mean = mean(sum_comp)) %>%
           ungroup())
  
}

extract_total_movement_per_county_single_week <- function(filter_week, movement_data, movement = "all", include_local = FALSE){
  # movement can be "in", "out", "all"
  
  movements_filtered <- movement_data %>%
    filter(week == filter_week)
  print(paste("Total number of movements in", filter_week, ":", nrow(movements_filtered)))
  
  # 2. Extract in country movements 
  in_country_movements <- movements_filtered %>%
    filter(complete.cases(.)) %>%
    dplyr::select(week, kreis1, kreis2, travellers)
  
  if(!include_local){
    print("Removing self-loops...")
    in_country_movements <- in_country_movements %>%
      filter(kreis1 != kreis2) # deactivate this to include local movement
  }
  
  
  # TODO this is where I would have to implement the threshold
  
  movements_out <- in_country_movements %>%
    group_by(week, kreis1) %>%
    summarise(sum_out = sum(travellers)) %>%
    ungroup() %>%
    dplyr::select(week, kreis = kreis1, sum = sum_out)
  
  movements_in <- in_country_movements %>%
    group_by(week, kreis2) %>%
    summarise(sum_in = sum(travellers)) %>%
    ungroup() %>%
    dplyr::select(week, kreis = kreis2, sum = sum_in)
  
  if(movement == "all"){
    output <- movements_out %>%
      dplyr::union(movements_in) %>%
      group_by(week, kreis) %>%
      summarise(sum_comp = sum(sum))
  }
  if(movement == "out"){
    output <- movements_out %>%
      mutate(sum_comp = sum)
  }
  if(movement == "in"){
    output <- movements_in %>%
      mutate(sum_comp = sum)
  }
  
  return(output %>%
           group_by(kreis) %>%
           summarise(mean = mean(sum_comp)) %>%
           ungroup())
  
}
