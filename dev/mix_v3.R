# Rewrite of mix_v2 to use non-zero means

## log density  f(|x| ; μ,σ) = φ(|x|; μ,σ) + φ(|x|; −μ,σ)
log_dmixabs <- function(x, p, mu, s) {
  log_p <- log(p)
  
  log_phi_pos <- outer(x, seq_along(p), function(xi, j)
    dnorm(xi,  mu[j], s[j], log = TRUE))
  log_phi_neg <- outer(x, seq_along(p), function(xi, j)
    dnorm(xi, -mu[j], s[j], log = TRUE))
  
  log_fold  <- rowLogSumExp(log_phi_pos)            # log[φ(+)+φ(−)]
  rowLogSumExp(t(t(log_fold) + log_p))              # mix over components
}

## log CDF  F(|x| ; μ,σ) = Φ((|x|−μ)/σ) + Φ((|x|+μ)/σ) − 1
log_pmixabs <- function(x, p, mu, s) {
  log_p <- log(p)
  
  F_mat <- outer(x, seq_along(p), function(xi, j)
    pnorm((xi - mu[j]) / s[j]) + pnorm((xi + mu[j]) / s[j]) - 1)
  
  log_F <- log(pmax(F_mat, .Machine$double.eps))
  rowLogSumExp(t(t(log_F) + log_p))
}




loglik_v2 <- function(theta, z, truncated, k, weights) {

  p_raw <- theta[1:(k - 1)]
  p     <- c(p_raw, 1 - sum(p_raw))
  mu    <- theta[  (k):(2*k - 1) ]
  s     <- theta[(2*k):(3*k - 1) ]
  omega <- theta[3*k]
  
  if (any(p <= 0) || any(mu < 0) || any(s <= 0) || omega <= 0)
    return(1e12)
  
  abs_z <- abs(z)
  log_d <- log_dmixabs(abs_z, p, mu, s)
  log_F <- log_pmixabs(abs_z, p, mu, s)
  
  F_cut <- sum(p * (pnorm((1.96 - mu)/s) + pnorm((1.96 + mu)/s) - 1))
  B1 <- 1 - F_cut
  B2 <- F_cut
  log_norm <- log(B1 + omega * B2)
  
  log_omega <- log(omega)
  
  ## build the four cases
  low   <- abs_z < 1.96
  up    <- !low
  trunc <- truncated
  notr  <- !trunc
  
  log_lik <- numeric(length(z))
  
  # |z| < 1.96   &  truncated
  idx <- which(low & trunc)
  if (length(idx)) log_lik[idx] <- log_omega + log_F[idx]
  
  # |z| < 1.96   &  not truncated
  idx <- which(low & notr)
  if (length(idx)) log_lik[idx] <- log_omega + log_d[idx]
  
  # |z| ≥ 1.96   &  not truncated
  idx <- which(up & notr)
  if (length(idx)) log_lik[idx] <- log_d[idx]
  
  # |z| ≥ 1.96   &  truncated
  idx <- which(up & trunc)
  if (length(idx)) log_lik[idx] <- log1mexp(log_F[idx])
  
  ## subtract the global normalising constant
  log_lik <- log_lik - log_norm
  
  ## return *minus* the weighted log-likelihood (for minimisers)
  -sum(weights * log_lik)
}
