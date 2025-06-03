# Fitting mixture distribution to a large set of z-values

# We fit a 4-part zero-mean normal mixture distribution to the z-statistics. 
# Or rather, we fit a mixture of half-normals to the absolute z-statistics.

# * We assume that z-statistics less than 1.96 may have smaller probability of 
#   being observed due to publication bias and/or the file drawer effect. We use a 
#   parameter $\omega$ to represent the relative risk:
#   $$\omega = \frac{P(\text{observe} \mid |z| < 1.96)}{P(\text{observe} \mid |z| \geq 1.96)}.$$

# * We weight each z-statistic by the inverse of the number of z-statistics 
#   in the same study.

# * We also handle truncation. Truncated z-stats below 1.96 are assumed to be 
#   truncated above, and truncated z-stats above 1.96 are assume to be truncated below. 


## Helper functions for mixture of normals -----
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




# likelihood of mixtures -----
# (log likelihood)
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
  
  lik1 <- (abs_z <  1.96)*( truncated)*omega*p_mix_eval
  lik2 <- (abs_z <  1.96)*(!truncated)*omega*d_mix_eval
  lik3 <- (abs_z >= 1.96)*(!truncated)*d_mix_eval
  lik4 <- (abs_z >= 1.96)*( truncated)*(1-p_mix_eval)
  
  lik <- (lik1+lik2+lik3+lik4)/(B1 + omega*B2)
  
  return(-sum(weights*log(lik)))   # *minus* the weighted log lik
}



# Mixture function that makes use of constrOptim() -----

optimise_mixture = function(z,truncated,k=4,weights){
  # The feasible region is defined by ui %*% par - ci >= 0
  ui = c(rep(-1,(k-1)),rep(0,k),0)         # (k-1) mixture props sum to < 1
  ui = rbind(ui,cbind(diag(2*k)))          # k stdevs > 1 and and omega > 0
  ci = c(-1,rep(0,k-1),rep(1,k),0)
  
  # set starting value and optimize 
  theta0=c(rep(1/k,(k-1)),c(1.2,2:k),1) #default chosen by Erik
  opt=constrOptim(theta=theta0,
                  f=loglik,
                  ui=ui,
                  ci=ci,
                  method = "Nelder-Mead",
                  z=abs(z),
                  weights=weights,
                  truncated=truncated,
                  k=k,
                  control=list(maxit=10^4))
  
  # collect the results
  p=c(opt$par[1:(k-1)],1-sum(opt$par[1:(k-1)]))  # mixture proportions
  sigma=opt$par[k:(2*k-1)]                       # mixture sds
  omega=opt$par[2*k]
  m=rep(0,k)                                     # mixture means
  df=data.frame(p=p,m=m,sigma=sigma,omega=omega)
  return(df)
}


# Mixture function v2: allows for use of BFGS optimiser and faster loglik code -----

optimise_mixture_v2 <- function(z, truncated, 
                                k = 4, weights,
                             optimiser = c("Nelder-Mead", "L-BFGS")) {
  
  optimiser <- match.arg(optimiser)
  z <- abs(z)
  
  ## starting values
  theta0 <- c(rep(1 / k, k - 1), c(1.2, 2:k), 1)
  
  if (optimiser == "Nelder-Mead") {
    
    ## linear-constraint setup as before
    ui <- c(rep(-1, k - 1), rep(0, k), 0)
    ui <- rbind(ui, cbind(diag(2 * k)))
    ci <- c(-1, rep(0, k - 1), rep(1, k), 0)
    
    opt <- constrOptim(theta0, f = loglik,
                       ui = ui, ci = ci,
                       method = "Nelder-Mead",
                       z = z, truncated = truncated,
                       k = k, weights = weights,
                       control = list(maxit = 1e4))
    
    par <- opt$par
    
  } else { # ---- L-BFGS path ----
    
    ## simple box bounds;   p_k = 1 − Σ p_i  enforced via penalty inside loglik_v2
    lower <- c(rep(0, k - 1),  rep(1, k),  0)         # p_i ≥ 0, σ ≥ 1, ω ≥ 0
    upper <- c(rep(1, k - 1),  rep(Inf, k + 1))
    
    opt <- optim(theta0, fn = loglik_v2,
                 z = z, truncated = truncated, k = k, weights = weights,
                 method = "L-BFGS-B",
                 lower = lower, upper = upper,
                 control = list(maxit = 1e4))
    
    par <- opt$par
  }
  
  ## unpack
  p     <- c(par[1:(k - 1)], 1 - sum(par[1:(k - 1)]))
  sigma <- par[k:(2 * k - 1)]
  omega <- par[2 * k]
  
  data.frame(p = p, m = 0, sigma = sigma, omega = omega)
}


# Functions for fitting and plotting of mixtures -----

# Wrapper around optimise_mixture_v2 which does some pre-processing of z's
fit_mixture <- function(z, truncated, ...){
  # Pre-processing: truncate z values at 20 and fix z=0
  if(is.null(truncated))
    truncated <- rep(FALSE, length(z))
  
  z.trunc=abs(z)
  ind <- which(z.trunc>20)
  z.trunc[ind] <- 20
  truncated[ind] <- TRUE  # commented out to save time
  if(length(ind) > 0)
    message(paste("Truncated", length(ind), "z values to 20"))
  
  ind <- which(z.trunc==0)
  z.trunc[ind] <- 0.5
  truncated[ind] <- TRUE
  if(length(ind) > 0)
    message(paste("Set", length(ind), "z=0 values to 0.5 (truncation)"))
  
  fit <- optimise_mixture_v2(z.trunc, truncated = truncated, ...)
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
