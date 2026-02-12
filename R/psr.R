# Given a fitted mixture, calculate things such as power, significance, replication

powsignrep <- function(fit, 
                       ss_multiplier = 1, #for checking scaled 
                       z_star = 0, #only keep z values greater than ...
                       N = 1e05) {
  
  snr <- sqrt(ss_multiplier)*rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  z   <- snr + rnorm(N)
  
  # I can use this to create a subset of z-values > 1,96 etc
  if(z_star > 0) {
    z <- z[abs(z) > z_star]
    while(length(z) < N) {
      snr <- sqrt(ss_multiplier)*rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
      z_new   <- snr + rnorm(N)
      z <- c(z, z_new[abs(z_new) > z_star])
    }
    z <- z[1:N]
  }
    
  power    <- (1 - pnorm(1.96, snr, 1)) + pnorm(-1.96, snr, 1)
  # Faster vectorised version of gap():
  pr <- gap_vec(z, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  data.frame(snr, z, power, row.names = NULL) %>%
    mutate(sgn = pr$sgn, rep = pr$rep)
}

  
# Vectorised version of gap() function written by chatGPT-5 to speed up
# computation and verified by WW to produce the same results as old code
gap_vec <- function(z, p, m, s_snr) {
  z <- abs(as.numeric(z))
  k <- length(p)
  p <- p / sum(p)
  s2 <- if (length(s_snr) == 1) rep(s_snr^2, k) else s_snr^2
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
  
  comp_rep <- pnorm(1.96, mean = mu, sd = SD_rep, lower.tail = FALSE)
  
  list(
    sgn = rowSums(w * pnorm(0, mean = mu, sd = SD_sgn, lower.tail = FALSE)),
    rep = rowSums(w * comp_rep)
  )
}

gap_vec_fit <- function(z, fit) {
  out <- gap_vec(z, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  data.frame(sgn = out$sgn, rep = out$rep)
}
