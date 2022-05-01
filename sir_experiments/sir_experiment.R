run_experiment <- function(sir_params,
                           initial_state_values, 
                           pre_lockdown, 
                           movement_data, 
                           N_i, 
                           weeks, 
                           upper_limit = 53,
                           experiment = "observed",
                           movement_reduction = TRUE, 
                           structural_update = TRUE, 
                           trace = FALSE){
  
  times <- seq(from = 0, to = 7, by = 1)
  output <- initial_state_values
  final_result <- data.frame()
  
  # current_state_values <- initial_state_values
  P <- national_mobility(pre_lockdown, movement_data)
  N_ij <- N_i * P
  N_j <- colSums(N_ij)
  Q <- t(t(N_ij) / N_j)
  
  # Load kappa based on whether it was requested
  # kappa is the reduction in the contact rate as a linear factor of the reduction in movement
  if(movement_reduction){
    load("data/kappas_baseline_week38.Rdata")
    kappa <- kappas %>%
      select(time, kappa, county) %>%
      pivot_wider(time, names_from = county, values_from = kappa)

    if(experiment %in% c("partial_from_beginning", "only_partial")){
      kappa <- kappas %>%
        select(time, kappa, county) %>%
        filter(time <= 7*9) %>%
        pivot_wider(time, names_from = county, values_from = kappa)
    }
    
    if(experiment == "extended_at_partial"){
      kappa <- kappas %>%
        select(time, kappa, county) %>%
        pivot_wider(time, names_from = county, values_from = kappa) %>%
        filter(time <= 7 * 7 | time >= 14 * 7) %>%
        mutate(time = if_else(time == 98, 7 * 8, time),
               time = if_else(time == 105, 8 * 8, time))
    }
    
    if(experiment == "extended_no_partial"){
      kappa <-  kappas %>%
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
      
  } else {
    # This basically just loads kappa's all as 1 in order to use the same ode implementation
    # without the effect of the mobility reduction
    ones <- rep(1, num_counties)
    kappa <-  matrix(
      # time, kappa by county, 
      c(  0, ones,
          50, ones,
          100, ones), ncol=num_counties + 1, byrow=TRUE
    ) 
  }
  
  # Define the starting parameters for the ODE
  # beta and gamma are used form the user input
  params <- list(
    beta = sir_params$beta,
    gamma = sir_params$gamma,
    P = P,
    Q = Q,
    kappa = kappa,
    time_mult = 0)
  
  # Main loop to update the structure of the mobility network each week
  for(i in 1:nrow(weeks)){
    row <- weeks[i,]
    
    if(row$year == 2020 & row$week_number > 38 & row$week_number <= upper_limit){
      
      if(structural_update){
        if(trace){
          print("Updating initial state...")
        }
        print(paste("Updateting with week", row$week))
        # Get sub-populations for next week
        P_new <- national_mobility(row$week, movement_data)
        
   
        N_ij_new <- N_i * P_new
        N_j_new <- colSums(N_ij_new)
        Q_new <- t(t(N_ij_new) / N_j_new)
        
        # Update initial values according to new sub-populations
        current_state_values <- update_sub_populations(output, N_ij, N_ij_new, trace)
        
        
        P <- P_new
        Q <- Q_new
        N_ij <- N_ij_new
        
        params$P <- P
        params$Q <- Q
      } else{
        print("Update state without structural update ...")
        current_state_values <- output
      }
    } else{
      # if(trace){
      print("Update state without structural update ...")
      # }
      current_state_values <- output
    }
    params$time_mult <- row$update_mult
    print(params$time_mult)
    print(as.numeric(approxTime(kappa, 0 + 7 * params$time_mult, rule = 2, method = "linear")[-1]))
    print(as.numeric(approxTime(kappa, 7 + 7 * params$time_mult, rule = 2, method = "linear")[-1]))
    
    if(trace){
      print(paste("Running SIR model", row$week, "..."))
    }
    result_test <- as.data.frame(ode(y = current_state_values,
                                     time = times,
                                     func = commute_sir_model,
                                     parms = params,
                                     method = "ode45"))
    
    if(trace){
      print("Updating results...")
    }
    
    result_long_test <- melt(result_test, id = "time") 
    final_result <- rbind(final_result, result_long_test %>%
                            mutate(time = time + 7 * row$mult))
    
    output_last <- result_long_test %>%
      filter(time == 7)
    
    output <- output_last$value
    names(output) <- output_last$variable
  }
  return(list(result = final_result, P = P, Q = Q))
}
