# Given a fitted mixture, calculate things such as power, significance, replication

powsignrep <- function(fit, ss_multiplier = 1, N = 1e05) {
  snr <- sqrt(ss_multiplier)*rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
  z   <- snr + rnorm(N)
  power    <- (1 - pnorm(1.96, snr, 1)) + pnorm(-1.96, snr, 1)
  pr <- gap_vec(z, p = fit$p, m = fit$m, s_snr = fit$sigma_SNR)
  # Faster vectorised version
  data.frame(snr, z, power, row.names = NULL) %>%
    mutate(sgn = pr$sgn, rep = pr$rep)
}

  
# Vectorised version of gap() function written by chatGPT-5 to speed up
# computation and verified by WW to produce the same results as old code
gap_vec <- function(z, p, m, s_snr) {
  z <- abs(as.numeric(z)); k <- length(p); p <- p / sum(p)
  s2 <- if (length(s_snr) == 1) rep(s_snr^2, k) else s_snr^2
  sp <- sqrt(s2 + 1)                      # SD(Z | comp)
  a  <- s2 / (s2 + 1); b <- m / (s2 + 1)  # mu_i(z) = a_i z + b_i
  tau2 <- s2 / (s2 + 1); tau <- sqrt(tau2)
  sd_rep <- sqrt(tau2 + 1)                # SD(Z_rep | z, comp)
  
  N <- length(z)
  loglike <- vapply(seq_len(k), function(j) dnorm(z, m[j], sp[j], log=TRUE), numeric(N))
  logw <- sweep(loglike, 2, log(p), `+`)
  w <- exp(logw - matrixStats::rowLogSumExps(logw))       # N×k
  
  mu <- outer(z, a) + matrix(b, N, k, byrow=TRUE)
  SD <- matrix(sd_rep, N, k, byrow=TRUE)
  
  # one-sided replication prob to match gap(): P(Z_rep > 1.96)
  comp_rep <- pnorm(1.96, mean=mu, sd=SD, lower.tail=FALSE)
  
  list(
    sgn = rowSums(w * pnorm(0, mean=mu, sd=matrix(tau, N, k, byrow=TRUE), lower.tail=FALSE)),
    rep = rowSums(w * comp_rep)
  )
}
