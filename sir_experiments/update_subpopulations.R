update_sub_populations <- function(current_state, N_t, N_t1, trace = FALSE){
  if(trace){
    print("Updating sub-populations...")
  }
  
  s <- current_state[1:(length(current_state)/2)] # susceptible proportion for the sub-populations (length = 398*398)
  s_matrix = matrix(s, nrow = sqrt(length(s)), byrow = TRUE) # susceptible matrix
  
  
  j <- current_state[(length(current_state)/2+1):length(current_state)] # infected proportion for the sub-populations (length = 398*398)
  j_matrix <- matrix(j, nrow = sqrt(length(j)), byrow = TRUE)
  
  N_diff <- N_t1 - N_t
  
  N_diff_neg <- N_diff
  N_diff_neg[N_diff_neg > 0] <- 0
  N_diff_neg <- abs(N_diff_neg)
  
  N_diff_pos <- N_diff
  N_diff_pos[N_diff_pos < 0] <- 0
  
  s_change_neg <- s_matrix * N_diff_neg
  # view(s_change_neg)
  j_change_neg <- j_matrix * N_diff_neg
  # view(j_change_neg)
  
  s_change_pos <- rowSums(s_change_neg) / rowSums(N_diff_neg) * N_diff_pos
  # view(s_change_pos)
  j_change_pos <- rowSums(j_change_neg) / rowSums(N_diff_neg) * N_diff_pos
  
  
  S <- s_matrix * N_t
  I <- j_matrix * N_t
  
  S <- S - s_change_neg + s_change_pos
  I <- I - j_change_neg + j_change_pos
  
  
  S <- S / N_t1
  S[is.nan(S)] <- 0
  S <- as.vector(t(S))
  names(S) <- names(s)
  
  I <- I / N_t1
  I[is.nan(I)] <- 0
  I <- as.vector(t(I))
  names(I) <- names(j)
  
  return(c(S, I))
}
