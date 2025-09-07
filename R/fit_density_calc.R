fit_density_calc_abs <- function(fit, x = seq(0, 10, by = 0.01), c_star = 1.96) {
  omega <- fit$omega[1]
  B1    <- 2 * pmix(-c_star, p = fit$p, m = fit$m, s = fit$sigma)
  B2    <- 1 - B1
  
  density_func <- function(x, sigma = fit$sigma)
    2 * (omega * (x < c_star) + (x >= c_star)) *
      dmix(x, p = fit$p, m = fit$m, s = sigma) / (B1 + omega * B2)
  corrected_density_func <- function(x, sigma = fit$sigma) #without selection model
    2 * dmix(x, p = fit$p, m = fit$m, s = sigma) 
  
  xmin <- min(x); xmax <- max(x)
  E_abs_z <- integrate(function(x) x * corrected_density_func(x), 
                       lower = xmin, upper = min(100, xmax), rel.tol = 1e-6)$value
  
  list(df = tibble(x = x, 
                   fz = density_func(x),
                   f_snr = density_func(x, fit$sigma_SNR),
                   corrected_fz = corrected_density_func(x),
                   corrected_f_snr = corrected_density_func(x, fit$sigma_SNR)
                   ), 
       E_abs_z = E_abs_z)
}

fit_density_calc <- function(fit, x = seq(-10, 10, length = 800), c_star = 1.96) {
  corrected_density_func <- function(x, sigma = fit$sigma)
    dmix(x, p = fit$p, m = fit$m, s = sigma) 
  
  list(df = tibble(x = x, 
                   corrected_fz = corrected_density_func(x),
                   corrected_f_snr = corrected_density_func(x, fit$sigma_SNR)
                   ))
}
