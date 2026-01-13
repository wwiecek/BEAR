# old version, for posterity
loglik_orig <- function(theta, z, truncated, k=4, weights) {
  # all of these functions are designed to work with |z|
  abs_z <- abs(z)
  
  # theta is composed of: 
  # p = probabilities of mixture vector (k-1)
  # s = scales of each mixture components (k)
  # omega = relative probability of reporting
  
  p     <- c(theta[1:(k-1)], 1-sum(theta[1:(k-1)]))
  s     <- theta[k:(2*k-1)]
  omega <- theta[2*k]
  
  # means of mixtures are set to zeroes by default
  m     <- rep(0,k)
  
  B1 <- 1-pmixabs(1.96,p,m,s)    # prob. |z|>1.96
  B2 <- 1-B1                     # prob. |z|<1.96
  
  # For minimally faster operation, evaluate these once
  p_mix_eval <- pmixabs(abs_z,p,m=m,s=s)
  d_mix_eval <- dmixabs(abs_z,p,m=m,s=s)
  
  # it's common to have truncated == 1 and abs_z == 1.959964,
  # so setting the inequality at 1.96 would "flip" the truncation
  lik1 <- (abs_z <  1.9599)*( truncated)*omega*p_mix_eval
  lik2 <- (abs_z <  1.9599)*(!truncated)*omega*d_mix_eval
  lik3 <- (abs_z >= 1.9599)*(!truncated)*d_mix_eval
  lik4 <- (abs_z >= 1.9599)*( truncated)*(1-p_mix_eval)
  
  lik <- (lik1+lik2+lik3+lik4)/(B1 + omega*B2)
  
  return(-sum(weights*log(lik)))   # *minus* the weighted log lik
}
