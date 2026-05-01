# Preserved historical mixture helpers.
# Source this file explicitly when comparing old exploratory code paths.

qfun = function(q,p,m,s)   # quantile function scalar q
  uniroot(function(x) pmix(x,p,m,s)-q, interval=c(-20,20))$root

qmix = function(q,p,m,s)   # quantile function vector q
  sapply(q, function(q) qfun(q,p=p,m=m,s=s) )

qfunabs = function(q,p,m,s) # quantile function scalar q
  uniroot(function(x) pmixabs(x,p,m,s)-q, interval=c(0,20))$root

qmixabs = function(q,p,m,s) # quantile function vector q
  sapply(q, function(q) qfunabs(q,p=p,m=m,s=s))

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

# Older absolute-z mixture plot preserved for historical exploratory scripts.
plot_mixture_v3 <- function(fit, dt, nm = "", col = "black", xmax = 10, nbreaks = 25, ymax = 0.6) {
  den_calc <- fit_density_calc_abs(fit, x = seq(0, min(500, max(abs(dt$z))), by = 0.1))
  df <- den_calc$df
  br <- c(seq(0, xmax, length = nbreaks), max(abs(dt$z)))
  omega_val <- round(fit$omega[1], 2)
  e_abs_z_val <- round(den_calc$E_abs_z, 1)
  lab <- sprintf("atop(omega==%s, E(group('|',z,'|'))==%s)", omega_val, e_abs_z_val)

  ggplot() +
    geom_histogram(
      data = dt %>% mutate(x = abs(z)),
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = br, fill = col, alpha = 0.25, colour = NA
    ) +
    geom_line(data = df, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    coord_cartesian(xlim = c(-0.1, xmax + 0.1), ylim = c(0, ymax)) +
    labs(x = NULL, y = NULL, title = nm) +
    theme_bw() +
    theme(plot.title = element_text(size = 8),
          axis.text = element_text(size = 7),
          legend.position = "none") +
    annotate("text", x = 7, y = ymax - 0.075, label = lab, parse = TRUE, size = 2.5)
}

# Erik's function that used to do analysis within Rmd. Current scripts
# precalculate mixtures before plotting reports.
fit_and_plot = function(z,truncated,k=4,weights){
  operator <- ifelse(truncated, "<", "=")
  fit <- fit_mixture(z = z, operator = operator, k = k, weights = weights)
  ggp <- plot_mixture(fit, z, weights)
  print(ggp)
  return(fit)
}
