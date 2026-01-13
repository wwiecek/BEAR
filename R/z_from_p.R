# This mini function tries to deal with very, very small p values and 
# also performs a bit of truncation by default
z_from_p <- function(p, truncate = 100) {
  z <- qnorm(1 - p/2)
  # extra precision recommended by chatGPT
  small_p <- !is.na(p) & p < 1e-15
  z[small_p] <- sqrt(-2 * log(p[small_p]/2))
  z[!is.na(p) & p <= 0] <- truncate
  z[!is.na(p) & p >= 1] <- 0
  z
}