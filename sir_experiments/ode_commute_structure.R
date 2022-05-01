# DEFINTION OF THE SYSTEM OF ODEs USED FOR THE SIR MODEL
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
commute_sir_model <- function(time, state, parameters) {
  with(as.list(c(parameters)), {
    kappa_t <- as.numeric(approxTime(kappa, time + 7 * time_mult, rule = 2, method = "linear")[-1])
    
    # kappa_t <- as.numeric(approxTime(kappa, time, rule = 2, method = "linear")[-1])
    
    # parameters: P = flux matrix (outbound), Q = incoming proportion (matrix), beta (scalar), gamma(scalar)
    
    s <- state[1:(length(state)/2)] # susceptible proportion for the sub-populations (length = 398*398)
    s_matrix = matrix(s, nrow = sqrt(length(s)), byrow = TRUE) # susceptible matrix
    
    j <- state[(length(state)/2+1):length(state)] # infected proportion for the sub-populations (length = 398*398)
    j_matrix <- matrix(j, nrow = sqrt(length(j)), byrow = TRUE)
    # print()
    
    one <- matrix(rep(1, length(s)), nrow = sqrt(length(s))) # matrix of ones to get rowsums
    
    # every column is the same - row only contains the same value (kappa can just be multiplied)
    lambda_home <- (kappa_t * beta)/2 * ((P * j_matrix) %*% one)
    lambda_work <- t((kappa_t * beta)/2 * t(one %*% (Q * j_matrix)))
    
    lambda <- lambda_home + lambda_work
    
    # I need to transpose as as.vector() creates appends columns to get the vector
    # I want to have to row because then counties are together
    ds <- as.vector(t(-lambda * s_matrix)) 
    dj <- as.vector(t(lambda * s_matrix - gamma * j_matrix)) 
    
    
    return(list(c(ds, dj)))
  })
}
