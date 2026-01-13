# like mix_v3 but allowing for negative values of z

## log density of a k-component normal mixture  Σ p_j φ(x; μ_j, σ_j)
log_dmix <- function(x, p, mu, s) {
  log_p <- log(p)
  log_d <- outer(x, seq_along(p), function(xi, j)
    dnorm(xi, mu[j], s[j], log = TRUE))
  rowLogSumExp(t(t(log_d) + log_p))
}

## log CDF  F(x) = Σ p_j Φ((x − μ_j)/σ_j)
log_pmix <- function(x, p, mu, s) {
  log_p <- log(p)
  Fmat  <- outer(x, seq_along(p), function(xi, j)
    pnorm((xi - mu[j]) / s[j]))
  log_F <- log(pmax(Fmat, .Machine$double.eps))
  rowLogSumExp(t(t(log_F) + log_p))
}

## log Pr(|Z| ≤ t) and log Pr(|Z| ≥ t) for one threshold (vectorised in t)
log_Pabs <- function(t, p, mu, s) {
  log_F_pos <- log_pmix( t, p, mu, s)    # log Pr(Z ≤  t)
  log_F_neg <- log_pmix(-t, p, mu, s)    # log Pr(Z ≤ −t)
  log_in    <- log_diff_exp(log_F_pos, log_F_neg)  # log Pr(|Z| ≤ t)
  log_out   <- log1mexp(log_in)                     # log Pr(|Z| ≥ t)
  list(in  = log_in,   out = log_out)
}

log_diff_exp <- function(a, b) {
  b_gt_a <- b > a
  if (any(b_gt_a, na.rm = TRUE))
    stop("log_diff_exp: require a ≥ b")
  a + log1p(-exp(b - a))
}

loglik_signed <- function(theta, z, truncated, k, weights) {
  
  ## unpack θ
  p_raw <- theta[1:(k - 1)]
  p     <- c(p_raw, 1 - sum(p_raw))
  mu    <- theta[      k:(2 * k - 1) ]
  s     <- theta[(2*k):(3 * k - 1) ]
  omega <- theta[3 * k]
  
  ## validity check
  if (any(p <= 0) || any(s <= 0) || omega <= 0) return(1e12)
  
  ## pre-compute log pdf & log CDF at each observed z
  log_d <- log_dmix(z,       p, mu, s)
  log_F <- log_pmix(z,       p, mu, s)
  
  ## pre-compute tails for |Z| cut-point 1.96
  cut    <- 1.96
  Pabs   <- log_Pabs(cut, p, mu, s)
  log_B1 <- Pabs$out    # log Pr(|Z| ≥ 1.96)
  log_B2 <- Pabs$in     # log Pr(|Z| <  1.96)
  log_norm <- logSumExp(log_B1, log(omega) + log_B2)
  
  ## categorical masks
  abs_z <- abs(z)
  low   <- abs_z <  cut
  up    <- !low
  trunc <- truncated
  notr  <- !trunc
  pos   <- z >= 0
  
  ## log-likelihood vector
  ll <- numeric(length(z))
  
  # 1) |z| < 1.96 & truncated  → Pr(|Z| ≤ |z|)
  idx <- which(low & trunc)
  if (length(idx)) {
    P_in  <- log_Pabs(abs_z[idx], p, mu, s)$in
    ll[idx] <- log(omega) + P_in
  }
  
  # 2) |z| < 1.96 & observed   → pdf
  idx <- which(low & notr)
  if (length(idx)) ll[idx] <- log(omega) + log_d[idx]
  
  # 3) |z| ≥ 1.96 & observed   → pdf
  idx <- which(up & notr)
  if (length(idx)) ll[idx] <- log_d[idx]
  
  # 4) |z| ≥ 1.96 & truncated  → one-sided tail (sign-specific)
  idx_pos <- which(up & trunc &  pos)
  if (length(idx_pos)) {
    ll[idx_pos] <- log1mexp(log_pmix(z[idx_pos], p, mu, s))   # Pr(Z ≥ z)
  }
  idx_neg <- which(up & trunc & !pos)
  if (length(idx_neg)) {
    ll[idx_neg] <- log_pmix(z[idx_neg], p, mu, s)             # Pr(Z ≤ z)
  }
  
  ## subtract global normalising constant
  -sum(weights * (ll - log_norm))
}

optimise_mixture_v3 <- function(z, truncated,
                                k = 4, weights) {
  # L-BFGS with signed mean μ
  
  ## θ = (p₁…p_{k-1}, μ₁…μ_k, σ₁…σ_k, ω)
  theta0 <- c(rep(0.9 / k, k - 1),            # p_i
              rep(0,        k),               # μ_i
              seq(1.2, length.out = k),       # σ_i
              0.8)                            # ω
  
  lower  <- c(rep(0,  k - 1),                 # p_i ≥ 0
              rep(-Inf,   k),                 # μ_i free (can be neg.)
              rep(1,      k),                 # σ_i ≥ 1
              0)                              # ω ≥ 0
  upper  <- c(rep(1,  k - 1),
              rep(Inf, 2 * k + 1))            # rest unbounded +
  
  opt <- optim(theta0, fn = loglik_signed,
               z = z, truncated = truncated,
               k = k, weights = weights,
               method = "L-BFGS-B",
               lower = lower, upper = upper,
               control = list(maxit = 1e4))
  
  par   <- opt$par
  p     <- c(par[1:(k - 1)], 1 - sum(par[1:(k - 1)]))
  mu    <-  par[      k:(2 * k - 1) ]
  sigma <-  par[(2*k):(3 * k - 1)]
  omega <-  par[ 3 * k ]
  
  data.frame(p = p, m = mu, sigma = sigma, omega = omega)
}


