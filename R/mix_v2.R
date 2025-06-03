# Fitting mixture distribution to a large set of z-values
# see mix.R for context
# these are rewrites of a few crucial functions to improve speed

## stable log-sum-exp by row
rowLogSumExp <- function(mat) {
  if (requireNamespace("matrixStats", quietly = TRUE)) {
    return(matrixStats::rowLogSumExps(mat))
  }
  m <- apply(mat, 1, max)
  m + log(rowSums(exp(mat - m)))
}

## log(1 − exp(a))  for a ≤ 0   (vectorised, stable)
# log1mexp <- function(a) {
#   # split at log(0.5) ≈ −0.693… (Abramowitz & Stegun 7.1.27)
#   i <- a > log(0.5)
#   out <- numeric(length(a))
#   out[i]  <- log1p(-exp(a[i]))       # a close to 0
#   out[!i] <- log(-expm1(a[!i]))      # a far from 0
#   out
# }

log1mexp <- function(a) {            # vectorised, works for any finite a
  ok <- !is.na(a)
  out <- a                         # allocate
  a   <- pmin(a, 0)                # force a ≤ 0; anything >0 → 0
  i   <- a > log(0.5)
  out[ ok &  i] <- log1p(-exp(a[ ok &  i]))
  out[ ok & !i] <- log(-expm1(a[ ok & !i]))
  out
}

## log density of a half-normal mixture  f(|x|)
log_dmixabs <- function(x, p, s) {
  log_2  <- log(2)
  log_p  <- log(p)
  
  # matrix: rows = |x| values, cols = components
  log_dens <- outer(x, s, function(xi, sj)
    log_2 + dnorm(xi, mean = 0, sd = sj, log = TRUE))
  
  rowLogSumExp(t(t(log_dens) + log_p))   # add log weights then log-sum-exp
}

## log CDF of the |x| mixture   F(|x|)
log_pmixabs <- function(x, p, s) {
  log_p  <- log(p)
  
  # component CDFs:  F_j(|x|) = 2 Φ(|x|/σ_j) − 1
  cdf_mat <- outer(x, s, function(xi, sj)
    2 * pnorm(xi, mean = 0, sd = sj) - 1)
  
  eps <- .Machine$double.eps             # guard against log(0)
  log_cdf <- log(pmax(cdf_mat, eps))
  
  rowLogSumExp(t(t(log_cdf) + log_p))
}

loglik_v2 <- function(theta, z, truncated, k, weights) {
  
  abs_z <- abs(z)
  
  ## unpack the parameter vector
  p     <- c(theta[1:(k - 1)], 1 - sum(theta[1:(k - 1)]))
  s     <- theta[k:(2 * k - 1)]
  omega <- theta[2 * k]
  
  ## invalid simplex, non-positive σ or ω → return a large penalty
  if (any(p <= 0) || any(s <= 0) || omega <= 0) {
    return(1e12)                       # optimiser treats this as “bad”
  }
  
  ## pre-compute per-observation log f  and  log F
  log_d <- log_dmixabs(abs_z, p, s)
  log_F <- log_pmixabs(abs_z, p, s)
  
  ## normalising constant  B1 + ω B2   (scalar)
  F_cut <- sum(p * (2 * pnorm(1.96, mean = 0, sd = s) - 1))
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
