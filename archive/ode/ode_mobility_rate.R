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

# Define the ODE system
sir_model <- function(time, state, parameters) {
  with(as.list(c(parameters)), {
    # print(beta)
    # print(alpha_nat)
    # s_int <- as.numeric(approxTime(s_int_values, time, rule = 2, method = "linear")[-1])
    # j_int <- as.numeric(approxTime(j_int_values, time, rule = 2, method = "linear")[-1])
    # one_int <- rep(1, length(s_int))

    s <- state[1:398] # suseptible proportion for the state
    j <- state[399:796] # infected proportion for the state
    one <- rep(1, length(s)) # Vector of 1s for rowSum <- what is better?

    threshold_fun <- (j / epsilon)^10 / ((j / epsilon)^10 + 1) # nolint
    lambda <- contact_rate_adjstmnt(time) * beta * threshold_fun * j
    # print(lambda)

    # TODO could make movement into function
    # Defining the part of the ODE deals with national and international movement
    nat_mov_s <- nat_mob_rate_adjstmnt(time) * alpha_nat * (P %*% s - (P * s) %*% one) # nolint
    # int_mov_s <- int_mob_rate_adjstmnt(time) * alpha_int * (Q %*% s_int - (Q * s) %*% one_int) # nolint
    ds <- -lambda * s + nat_mov_s # + int_mov_s

    nat_mov_j <- nat_mob_rate_adjstmnt(time) * alpha_nat * (P %*% j - (P * j) %*% one) # nolint
    # int_mov_j <- int_mob_rate_adjstmnt(time) * alpha_int * (Q %*% j_int - (Q * j) %*% one_int) # nolint
    dj <- lambda * s  - gamma * j + nat_mov_j # + int_mov_j


    return(list(c(ds, dj)))
  })
}

# Solve the ODE system
# output <- ode(
#   y = initial_state_values,
#   times = time,
#   func = sir_model,
#   parms = parameters
# )
