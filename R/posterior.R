# Conditional distribution of SNR given z, when SNR ~ dmix(p,m,s)

posterior <- function(z,p,m,s) {
  s2=s^2
  p=p*dnorm(z,m,sqrt(s2+1))
  p <- p/sum(p)                  # conditional mixing probs
  pm <- z*s2/(s2+1) + m/(s2+1)   # conditional means
  pv <- s2/(s2+1)                # conditional variances
  ps <- sqrt(pv)                 # conditional std devs
  data.frame(p,pm,pv,ps)
}
