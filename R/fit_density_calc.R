build_abs_plot_grid <- function(x, c_star = 1.96) {
  x <- sort(unique(c(x, c_star)))
  x_low <- x[x < c_star]
  x_high <- x[x > c_star]
  
  tibble(
    x = c(x_low, c_star, c_star, x_high),
    threshold_side = c(
      rep("regular", length(x_low)),
      "low",
      "high",
      rep("regular", length(x_high))
    )
  )
}

fit_density_calc_abs <- function(fit, x = seq(0, 10, by = 0.01), c_star = 1.96) {
  omega <- fit$omega[1]
  B1    <- 2 * pmix(-c_star, p = fit$p, m = fit$m, s = fit$sigma)
  B2    <- 1 - B1
  norm_const <- B1 + omega * B2
  df <- build_abs_plot_grid(x, c_star = c_star)
  
  corrected_density_func <- function(x, sigma = fit$sigma) #without selection model
    2 * dmix(x, p = fit$p, m = fit$m, s = sigma) 
  
  density_func <- function(x, side, sigma = fit$sigma) {
    corrected <- corrected_density_func(x, sigma = sigma)
    mult <- ifelse(side == "low" | (side == "regular" & x < c_star),
                   omega / norm_const,
                   1 / norm_const)
    corrected * mult
  }
  
  xmin <- min(x)
  xmax <- max(x)
  E_abs_z <- integrate(function(x) x * corrected_density_func(x), 
                       lower = xmin, upper = min(100, xmax), rel.tol = 1e-6)$value
  
  list(df = df %>% mutate(
         fz = density_func(x, threshold_side),
         f_snr = density_func(x, threshold_side, fit$sigma_SNR),
         corrected_fz = corrected_density_func(x),
         aligned_corrected_fz = corrected_fz / norm_const,
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
