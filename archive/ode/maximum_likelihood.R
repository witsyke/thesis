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
fit_mle <- function(beta_start, alpha_start){
  return(mle2(sir_ll,
              start = list(lbeta = qlogis(beta_start), lalpha_nat = qlogis(alpha_start)),
              method = "Nelder-Mead",
              control = list(maxit = 1E5, trace = 1),
              trace = TRUE))
}
