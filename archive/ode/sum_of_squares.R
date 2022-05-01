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


fit_ssq <- function(beta_start, alpha_start, parameters_fit, case_data){
    return(optim(par = c(beta = beta_start, alpha_nat = alpha_start),
                          fn = SIR_SSQ,
                          dat = case_data,
                          params_fit = parameters_fit,
                  method = "Nelder-Mead",
                  control = list(trace = 2)))
}
