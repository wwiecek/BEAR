# Fitting mixture distribution to a large set of z-values
# original code by Erik Zwet that has been largely rewritten by Witold
# but is still almost 100% the idea by Erik 

# We fit a 4-part zero-mean normal mixture distribution to the z-statistics. 
# Or rather, we fit a mixture of half-normals to the absolute z-statistics.

# * We assume that z-statistics less than 1.96 may have smaller probability of 
#   being observed due to publication bias and/or the file drawer effect. We use a 
#   parameter $\omega$ to represent the relative risk:
#   $$\omega = \frac{P(\text{observe} \mid |z| < 1.96)}{P(\text{observe} \mid |z| \geq 1.96)}.$$

# * We weight each z-statistic by the inverse of the number of z-statistics 
#   in the same study.




## Helper functions for mixture of normals -----

# there are faster alternatives in explore/loglik/ folder, but this is fast enough for 
# the datasets that we are fitting

dmix = function(x,p,m,s) # density of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) dnorm(x,mean=m,sd=s)))

rmix = function(n,p,m,s){    # sample from a normal mixture
  d=rmultinom(n,1,p)
  rnorm(n, m%*%d, s%*%d)
}

pmix = function(x,p,m,s) # cdf of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) pnorm(x,mean=m,sd=s)))

qfun = function(q,p,m,s)   # quantile function scalar q
  uniroot(function(x) pmix(x,p,m,s)-q, interval=c(-20,20))$root

qmix = function(q,p,m,s)   # quantile function vector q
  sapply(q, function(q) qfun(q,p=p,m=m,s=s) )


## Helper functions for mixture of half-normals -----
dmixabs = function(x,p,m,s) # density of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) (dnorm(-x,mean=m,sd=s) + dnorm(x,mean=m,sd=s))))

pmixabs = function(x,p,m,s) # cumulative distr of |x|
  
  drop(p %*% sapply(x, function(x) pnorm(x,mean=m,sd=s) - pnorm(-x,mean=m,sd=s)))

qfunabs = function(q,p,m,s) # quantile function scalar q
  uniroot(function(x) pmixabs(x,p,m,s)-q, interval=c(0,20))$root

qmixabs = function(q,p,m,s) # quantile function vector q
  sapply(q, function(q) qfunabs(q,p=p,m=m,s=s))

# Some faster alternatives to speed up calculations in newer versions -----

pmixabs_fast <- function(x, p, s, m = rep(0, length(p))) {
  ## x : numeric vector of evaluation points (|x|)
  ## p : mixture weights, length k, must sum to 1
  ## m : component means,  length k
  ## s : component sds,    length k
  ## For every evaluation point x[i] compute the two-sided probability
  ##     P(-x[i] ≤ Z_j ≤ x[i]),    Z_j ~ N(m_j, s_j²)
  ## which is   Φ((x[i]-m_j)/s_j) – Φ((-x[i]-m_j)/s_j).
  z_hi <- outer(x, seq_along(p), function(xi, j) (xi - m[j]) / s[j])
  z_lo <- outer(x, seq_along(p), function(xi, j) (-xi - m[j]) / s[j])
  drop((pnorm(z_hi) - pnorm(z_lo)) %*% p)
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

## stable log-sum-exp by row
rowLogSumExp <- function(mat) {
  if (requireNamespace("matrixStats", quietly = TRUE)) {
    return(matrixStats::rowLogSumExps(mat))
  }
  m <- apply(mat, 1, max)
  m + log(rowSums(exp(mat - m)))
}

log1mexp <- function(a) {            # vectorised, works for any finite a
  ok <- !is.na(a)
  out <- a                         # allocate
  a   <- pmin(a, 0)                # force a ≤ 0; anything >0 → 0
  i   <- a > log(0.5)
  out[ ok &  i] <- log1p(-exp(a[ ok &  i]))
  out[ ok & !i] <- log(-expm1(a[ ok & !i]))
  out
}

# likelihood of mixtures -----

# A new modification of the likelihood function loglik,
# which allows for right and left 
# consoring of any z value
# Reimplemented by WW with chatGPT and validated to produce same
# likelihood as loglik() on non-truncated cases
loglik_op <- function(theta, z, operator,            
                      k, weights,
                      c_star = 1.96) {               
  
  abs_z <- abs(z)
  
  ## unpack parameters
  p     <- c(theta[1:(k - 1)], 1 - sum(theta[1:(k - 1)]))
  s     <- theta[k:(2 * k - 1)]
  omega <- theta[2 * k]
  
  if (any(p <= 0) || any(s < 1) || omega <= 0)   # σ≥1 as in the paper
    return(1e12)
  
  ## pre-compute 
  log_d      <- log_dmixabs(abs_z, p, s)
  F_abs      <- pmixabs_fast(abs_z, p, s)
  log_F      <- log(F_abs)
  # log_F      <- log_pmixabs_fast(abs_z, p, s)
  # F_abs      <- exp(log_F)
  log_omega  <- log(omega)
  
  ## constants at the publication frontier c* = 1.96 
  F_star <- sum(p * (2 * pnorm(c_star, 0, s) - 1))   #  F_Y(c*)
  B1     <- 1 - F_star                               #  = Pr(|Z|≥c*)
  B2     <- F_star                                   #  = Pr(|Z|< c*)
  log_norm <- log(B1 + omega * B2)                   #  global normaliser
  
  ## allocate result vector 
  log_lik <- numeric(length(z))
  
  ## case 1: exact value ("=") ------------------------------------------------
  idx <- operator == "="
  if (any(idx)) {
    low  <- abs_z[idx] < c_star
    log_lik[idx][ low] <- log_omega + log_d[idx][ low]
    log_lik[idx][!low] <-                 log_d[idx][!low]
  }
  
  ## case 2: inequality  "<" --------------------------------------------------
  idx <- operator == "<"
  if (any(idx)) {
    low_cut  <- abs_z[idx] <= c_star          # c_j on the *low* side
    high_cut <- !low_cut                      # c_j straddles / above
    
    ##  a)  [0, c_j) lies wholly below c*
    if (any(low_cut))
      log_lik[idx][ low_cut] <- log_omega + log_F[idx][ low_cut]
    
    ##  b)  interval crosses c*:    ω F(c*) + (F(c_j) − F(c*))
    if (any(high_cut)) {
      term <- omega * F_star + (F_abs[idx][ high_cut] - F_star)
      log_lik[idx][ high_cut] <- log(term)
    }
  }
  
  ## case 3: inequality  ">" --------------------------------------------------
  idx <- operator == ">"
  if (any(idx)) {
    high_cut <- abs_z[idx] >= c_star          # c_j on the *high* side
    low_cut  <- !high_cut                     # c_j below c*
    
    ##  a)  (c_j, ∞) entirely above c*
    if (any(high_cut))
      log_lik[idx][ high_cut] <- log1mexp(log_F[idx][ high_cut])
    
    ##  b)  interval crosses c*:  (1−F(c*)) + ω [F(c*) − F(c_j)]
    if (any(low_cut)) {
      term <- B1 + omega * (F_star - F_abs[idx][ low_cut])
      log_lik[idx][ low_cut] <- log(term)
    }
  }
  -sum(weights * (log_lik - log_norm))
}


# Mixture optimisation function -----

optimise_mixture <- function(z, z_operator, weights, k = 4,
                             legacy_mode = FALSE) {
  z <- abs(z)
  
  ## starting values (WW added large sigma as last entry, sigma = k too inflexible)
  theta0 <- c(rep(1 / k, k - 1), 
              c(1.2, if (k > 2) 2:(k-1), max(z)),
              .5)
  ui <- c(rep(-1, k - 1), rep(0, k), 0)
  ui <- rbind(ui, cbind(diag(2 * k)))
  ci <- c(-1, rep(0, k - 1), rep(1, k), 0) #sigma > 1!
  
  # Use this instead for omega < 1
  ui <- c(rep(-1, k - 1), rep(0, k), 0)
  ui <- rbind(ui, cbind(diag(2 * k)))
  ui <- rbind(ui, c(rep(0, 2*k-1), -1))  
  ci <- c(-1, rep(0, k - 1), rep(1, k), 0, -1) 
  
  if(!legacy_mode){
    opt <- constrOptim(theta0, f = loglik_op, ui = ui, ci = ci,
                       method = "Nelder-Mead",
                       z = z, operator = z_operator, 
                       k = k, weights = weights,
                       control = list(maxit = 1e4))
  }else{
    opt <- constrOptim(theta0, f = loglik_orig, ui = ui, ci = ci,
                       method = "Nelder-Mead",
                       z = z, truncated = (z_operator != "="), 
                       k = k, weights = weights,
                       control = list(maxit = 1e4))
  }
  cat("Objective function (divided by n): ", opt$value/length(z), "\n")
  par <- opt$par
  
  ## unpack
  p     <- c(par[1:(k - 1)], 1 - sum(par[1:(k - 1)]))
  sigma <- par[k:(2 * k - 1)]
  omega <- par[2 * k]
  
  data.frame(p = p, m = 0, sigma = sigma, omega = omega, 
             AIC = 2*k + 2*(opt$value),
             BIC = 2*log(sum(weights)) + 2*(opt$value))
}


# Functions for fitting and plotting of mixtures -----

# shorthand for df's
fit_mixture_df <- function(df, ...) fit_mixture(z = df$z, 
                                                operator = df$z_operator,
                                                weight = df$weights,
                                                ...)

# Wrapper around optimise_mixture which does some pre-processing of z's
fit_mixture <- function(z, 
                        operator, 
                        z_star = 25, 
                        ...){
  if(is.null(operator))
    operator <- rep("=", length(z))
  
  z <- abs(z)
  
  ind <- which(z==0)
  z[ind] <- 0.5
  operator[ind] <- "<"
  if(length(ind) > 0)
    message(paste("Changed", length(ind), "'z = 0' cases to 'z < 0.5' to avoid zero likelihood"))
  
  ind <- which(z > z_star)
  z[ind] <- z_star
  operator[ind] <- ">"
  if(length(ind) > 0)
    message(paste("Truncated", length(ind), "'z > ", z_star, "' cases to avoid numerical overflows"))
  
  fit <- optimise_mixture(z = z, z_operator = operator, ...)
  fit$sigma_SNR <- sqrt(fit$sigma^2 - 1)          # stdev van SNR
  
  return(fit)
}
