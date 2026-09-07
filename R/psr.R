# Given a fitted mixture, calculate things such as power, significance, replication

powsignrep <- function(fit,
                       ss_multiplier = 1, #for checking scaled
                       z_star = 0, #only keep z values greater than ...
                       z_threshold = 1.96,
                       N = 1e05) {

  draw_snr <- function(n) {
    sqrt(ss_multiplier) * rmix(n, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  }

  snr <- draw_snr(N)
  z <- snr + rnorm(N)

  # Draw directly from the mixture conditional on a significant original result.
  if(z_star > 0) {
    k <- length(fit$p)
    p <- fit$p / sum(fit$p)
    m <- sqrt(ss_multiplier) * fit$m
    s2 <- ss_multiplier * if(length(fit$sigma_SNR) == 1) {
      rep(fit$sigma_SNR^2, k)
    } else {
      fit$sigma_SNR^2
    }
    sp <- sqrt(s2 + 1)
    pr_positive <- pnorm(z_star, m, sp, lower.tail = FALSE)
    pr_negative <- pnorm(-z_star, m, sp)
    component <- sample.int(k, N, replace = TRUE,
                            prob = p * (pr_positive + pr_negative))
    positive <- runif(N) < pr_positive[component] /
      (pr_positive[component] + pr_negative[component])
    cdf_lower <- pnorm(z_star, m[component], sp[component])
    z <- ifelse(
      positive,
      qnorm(cdf_lower + runif(N) * (1 - cdf_lower), m[component], sp[component]),
      qnorm(runif(N) * pr_negative[component], m[component], sp[component])
    )
    a <- s2[component] / (s2[component] + 1)
    b <- m[component] / (s2[component] + 1)
    snr <- rnorm(N, a * z + b, sqrt(a))
  }

  power <- (1 - pnorm(z_threshold, snr, 1)) + pnorm(-z_threshold, snr, 1)
  # Faster vectorised version of gap():
  pr <- gap_vec(
    z,
    p = fit$p,
    m = fit$m,
    s_snr = fit$sigma_SNR,
    ss_multiplier = ss_multiplier,
    z_threshold = z_threshold
  )
  data.frame(snr, z, power, row.names = NULL) %>%
    mutate(sgn = pr$sgn, rep = pr$rep)
}

  
# Vectorised version of gap() function written by chatGPT-5 to speed up
# computation and verified by WW to produce the same results as old code
gap_vec <- function(z, p, m, s_snr, ss_multiplier = 1, z_threshold = 1.96) {
  z <- abs(as.numeric(z))
  k <- length(p)
  p <- p / sum(p)
  m <- sqrt(ss_multiplier) * m
  s2 <- ss_multiplier * if(length(s_snr) == 1) rep(s_snr^2, k) else s_snr^2
  sp <- sqrt(s2 + 1)
  a  <- s2 / (s2 + 1); b <- m / (s2 + 1)
  tau2 <- s2 / (s2 + 1); tau <- sqrt(tau2)
  sd_rep <- sqrt(tau2 + 1)
  
  N <- length(z)
  
  loglike <- vapply(seq_len(k), 
                    function(j) dnorm(z, m[j], sp[j], log = TRUE), 
                    numeric(N))
  # to work with scalar z
  if (is.null(dim(loglike))) loglike <- matrix(loglike, nrow = N, ncol = k)  
  
  logw <- sweep(loglike, 2, log(p), `+`)
  w <- exp(logw - matrixStats::rowLogSumExps(logw))
  
  mu <- outer(z, a) + matrix(b, N, k, byrow = TRUE)
  SD_rep <- matrix(sd_rep, N, k, byrow = TRUE)
  SD_sgn <- matrix(tau, N, k, byrow = TRUE)
  
  comp_rep <- pnorm(z_threshold, mean = mu, sd = SD_rep, lower.tail = FALSE)
  
  list(
    sgn = rowSums(w * pnorm(0, mean = mu, sd = SD_sgn, lower.tail = FALSE)),
    rep = rowSums(w * comp_rep)
  )
}

gap_vec_fit <- function(z, fit, ss_multiplier = 1, z_threshold = 1.96) {
  out <- gap_vec(
    z,
    p = fit$p,
    m = fit$m,
    s_snr = fit$sigma_SNR,
    ss_multiplier = ss_multiplier,
    z_threshold = z_threshold
  )
  data.frame(sgn = out$sgn, rep = out$rep)
}

# Vectorised posterior probability that PoS exceeds threshold conditional on z
pos_thresh_vec <- function(z, p, m, s_snr, pos_thresh = 0.8, alpha = 0.05) {
  z <- abs(as.numeric(z))
  k <- length(p)
  p <- p / sum(p)
  s2 <- if (length(s_snr) == 1) rep(s_snr^2, k) else s_snr^2
  sp <- sqrt(s2 + 1)
  a <- s2 / (s2 + 1)
  b <- m / (s2 + 1)
  tau <- sqrt(s2 / (s2 + 1))

  z_alpha <- qnorm(1 - alpha / 2)
  # two-sided power as a function of |snr|
  pos_fun <- function(snr_abs) {
    (1 - pnorm(z_alpha, snr_abs, 1)) + pnorm(-z_alpha, snr_abs, 1)
  }

  # Solve for |snr| that yields desired PoS threshold
  snr_thresh <- uniroot(function(x) pos_fun(x) - pos_thresh, c(0, 12))$root

  N <- length(z)
  loglike <- vapply(seq_len(k),
                    function(j) dnorm(z, m[j], sp[j], log = TRUE),
                    numeric(N))
  if (is.null(dim(loglike))) loglike <- matrix(loglike, nrow = N, ncol = k)

  logw <- sweep(loglike, 2, log(p), `+`)
  w <- exp(logw - matrixStats::rowLogSumExps(logw))

  mu <- outer(z, a) + matrix(b, N, k, byrow = TRUE)
  Tau <- matrix(tau, N, k, byrow = TRUE)

  comp_pos_thresh <- pnorm(snr_thresh, mean = mu, sd = Tau, lower.tail = FALSE) +
    pnorm(-snr_thresh, mean = mu, sd = Tau)

  rowSums(w * comp_pos_thresh)
}

pos_thresh_vec_fit <- function(z, fit, pos_thresh = 0.8, alpha = 0.05) {
  pos_thresh_vec(
    z,
    p = fit$p,
    m = fit$m,
    s_snr = fit$sigma_SNR,
    pos_thresh = pos_thresh,
    alpha = alpha
  )
}
