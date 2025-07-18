# Fitting mixture distribution to a large set of z-values
# original code by Erik Zwet that has been largely rewritten by Witold
# but is still very close to the ideas that Erik had

# We fit a 4-part zero-mean normal mixture distribution to the z-statistics. 
# Or rather, we fit a mixture of half-normals to the absolute z-statistics.

# * We assume that z-statistics less than 1.96 may have smaller probability of 
#   being observed due to publication bias and/or the file drawer effect. We use a 
#   parameter $\omega$ to represent the relative risk:
#   $$\omega = \frac{P(\text{observe} \mid |z| < 1.96)}{P(\text{observe} \mid |z| \geq 1.96)}.$$

# * We weight each z-statistic by the inverse of the number of z-statistics 
#   in the same study.

## Helper functions for mixture of normals -----

# there are faster alternatives in dev/ folder, but this is fast enough for 
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

# WW addition to speed up calculations in newer versions
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

# old version, for posterity
loglik <- function(theta, z, truncated, k=4, weights) {
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

# Mixture optimisation function -----
# Erik's old code makes use of constrOptim() and Nelder-Mead;
# v2 (rewritten by WW with chatGPT's help, validated by me)
# allows for use of BFGS optimiser and faster loglik code,
# as well as new truncation behaviour

optimise_mixture_v2 <- function(z, z_operator, weights, k = 4, 
                                optimiser = c("Nelder-Mead", "L-BFGS")) {
  
  optimiser <- match.arg(optimiser)
  z <- abs(z)
  
  ## starting values
  theta0 <- c(rep(1 / k, k - 1), c(1.2, 2:k),  1)
  
  if (optimiser == "Nelder-Mead") {
    ui <- c(rep(-1, k - 1), rep(0, k), 0)
    ui <- rbind(ui, cbind(diag(2 * k)))
    ci <- c(-1, rep(0, k - 1), rep(1, k), 0)
    
    opt <- constrOptim(theta0, f = loglik_op,
                       ui = ui, ci = ci,
                       method = "Nelder-Mead",
                       z = z, operator = z_operator, 
                       k = k, weights = weights,
                       control = list(maxit = 1e4))
    
    par <- opt$par
    
  } else { # ---- L-BFGS path ----
    
    # experimental choice for large datasets, I think it will often give a wrong answer
    
    ## simple box bounds;   p_k = 1 − Σ p_i  enforced via penalty inside loglik
    lower <- c(rep(0, k - 1),  rep(1, k),  0)         # p_i ≥ 0, σ ≥ 1, ω ≥ 0
    upper <- c(rep(1, k - 1),  rep(Inf, k + 1))
    
    opt <- optim(theta0, fn = loglik_op,
                 z = z, operator = z_operator, 
                 k = k, weights = weights,
                 lower = lower, upper = upper,
                 method = "L-BFGS-B",
                 control = list(maxit = 1e4))
    
    par <- opt$par
  }
  
  print(opt$value)
  
  ## unpack
  p     <- c(par[1:(k - 1)], 1 - sum(par[1:(k - 1)]))
  sigma <- par[k:(2 * k - 1)]
  omega <- par[2 * k]
  
  data.frame(p = p, m = 0, sigma = sigma, omega = omega)
}


# Functions for fitting and plotting of mixtures -----

# shorthand for df's
fit_mixture_df <- function(df) fit_mixture(z = df$z, 
                                           operator = df$z_operator,
                                           weight = df$weight)
  
# Wrapper around optimise_mixture_v2 which does some pre-processing of z's
fit_mixture <- function(z, operator, ...){
  if(is.null(operator))
    operator <- rep("=", length(z))
  
  z <- abs(z)

  ind <- which(z==0)
  z[ind] <- 0.5
  operator[ind] <- "<"
  if(length(ind) > 0)
    message(paste("Changed", length(ind), "'z = 0' cases to 'z < 0.5' to avoid zero likelihood"))
  
  ind <- which(z > 20)
  z[ind] <- 20
  operator[ind] <- ">"
  if(length(ind) > 0)
    message(paste("Truncated", length(ind), "'z > 20' cases to avoid numerical overflows"))
  
  fit <- optimise_mixture_v2(z = z, z_operator = operator, ...)
  fit$sigma_SNR <- sqrt(fit$sigma^2 - 1)          # stdev van SNR
  
  return(fit)
}

plot_mixture <- function(fit, z, weights) {
  
  omega=fit$omega[1]
  B1=2*pmix(-1.96,p=fit$p,m=fit$m,s=fit$sigma) # prob. |z|>1.96
  B2=1-B1
  
  df <- data.frame(z=abs(z),weights)
  
  df$fz <- 2*(omega*(df$z<1.96) + (df$z>=1.96))*
    dmix(z, p=fit$p, m=fit$m, s=fit$sigma)/(B1 + omega*B2)
  
  df$fz2=2*dmix(df$z, p=fit$p, m=fit$m, s=fit$sigma)
  
  ggplot(df, aes(x = z)) +
    geom_histogram(aes(y = after_stat(density),
                       weight=weights), 
                   color="black",
                   fill="white", breaks=seq(0,10,0.2)) +
    geom_line(aes(x=z,y=fz)) +
    geom_line(aes(x=z,y=fz2),color="red") +
    xlab("absolute z-statistic") + ylab('') + 
    xlim(0,10) + theme_bw()
}

# Erik's function that used to do analysis within Rmd, I am now suggesting we
# precalculate this before running Rmd reports
fit_and_plot = function(z,truncated,k=4,weights){
  fit <- fit_mixture(z,truncated,k=4,weights)
  ggp <- plot_mixture(fit, z, weights)
  print(ggp)
  return(fit)
}

