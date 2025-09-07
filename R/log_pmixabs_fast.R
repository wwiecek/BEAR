# log{ Φ(b) - Φ(a) } computed stably for vectors a<b
log_diff_pnorm <- function(a, b) {
  # Use symmetry so the 'upper' point is non-negative; improves stability
  swap <- b <= 0
  aa   <- ifelse(swap, -b, a)
  bb   <- ifelse(swap, -a, b)
  # Now bb >= 0
  logPhi_bb <- pnorm(bb, log.p = TRUE)
  logPhi_aa <- pnorm(aa, log.p = TRUE)
  # log(Φ(bb) - Φ(aa)) = logΦ(bb) + log1p( - exp(logΦ(aa) - logΦ(bb)) )
  logPhi_bb + log1p(-exp(logPhi_aa - logPhi_bb))
}

# Mixture of two-sided normal probabilities on the log scale
log_pmixabs_fast <- function(x, p, s, m = rep(0, length(p))) {
  stopifnot(length(p) == length(s), length(m) == length(p), all(s > 0))
  k <- length(p)
  p <- p / sum(p)                 # tolerate tiny summation error
  x <- abs(x)                     # function is defined for |x|
  
  # z-bounds per component
  z_hi <- outer(x, seq_len(k), function(xi, j) (xi - m[j]) / s[j])
  z_lo <- outer(x, seq_len(k), function(xi, j) (-xi - m[j]) / s[j])
  
  # component log-probabilities: log P_j(x)
  lcomp <- matrix(log_diff_pnorm(c(z_lo), c(z_hi)),
                  nrow = length(x), ncol = k, byrow = FALSE)
  
  # add log-weights and do row-wise log-sum-exp: log Σ_j p_j * P_j
  L     <- sweep(lcomp, 2, log(p), FUN = "+")
  maxL  <- apply(L, 1, max)
  maxL + log(rowSums(exp(L - maxL)))
}
