den_calc <- function(fit, x = grid_x, c_star = 1.96) {
  omega <- fit$omega[1]
  B1    <- 2 * pmix(-c_star, p = fit$p, m = fit$m, s = fit$sigma)
  B2    <- 1 - B1
  
  density_func <- function(x) {
    2 * (omega * (x < c_star) + (x >= c_star)) *
      dmix(x, p = fit$p, m = fit$m, s = fit$sigma) / (B1 + omega * B2)
  }
  corrected_density_func <- function(x) #without selection model
    2 * dmix(x, p = fit$p, m = fit$m, s = fit$sigma) 
  
  xmin <- min(x); xmax <- max(x)
  E_abs_z <- integrate(function(x) x * density_func(x), 
                       lower = xmin, upper = min(100, xmax), rel.tol = 1e-6)$value
  
  list(df = tibble(x = x, 
                   fz = density_func(x),
                   corrected_fz = corrected_density_func(x)), 
       E_abs_z = E_abs_z)
}



plot_mixture_v3 <- function(fit, dt, nm = "", col = "black", xmax = 10, nbreaks = 25, ymax = 0.6) {
  # calculate the density
  den_calc  <- den_calc(fit, x = seq(0, min(500, max(abs(dt$z))), by=0.1))
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
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    annotate("text", x = 7, y = ymax - 0.075, label = lab, parse = TRUE, size = 2.5)
}



plot_mixture_v4 <- function(fit, dt, nm = "", col = "black", color_map = NULL, 
                            xmax = 10, nbreaks = 25, ymax = 0.6, show_corrected = FALSE) {
  
  # Use color_map if provided and col is default
  if (!is.null(color_map) && col == "black" && "group" %in% names(dt)) {
    col <- color_map[as.character(dt$group[1])]
  }
  
  # calculate the density
  grid_max <- min(500, max(abs(dt$z))) #for stupidly large |z| calculation would crash!
  if(grid_max < xmax) grid_max <- xmax #this makes sure the density gets plotted past max(z) if needed
  den_calc_result <- den_calc(fit, x = seq(0, grid_max, by=0.1))
  df <- den_calc_result$df
  br <- c(seq(0, xmax, length = nbreaks), max(abs(dt$z)))
  omega_val <- round(fit$omega[1], 2)
  e_abs_z_val <- round(den_calc_result$E_abs_z, 1)
  # lab <- sprintf("atop(omega==%s, E(group('|',z,'|'))==%s)", omega_val, e_abs_z_val)
  # lab <- sprintf("atop(omega==%s, scriptstyle(E(group('|',z,'|'))==%s))", omega_val, e_abs_z_val)
  # lab <- sprintf("omega==%s~~E(group('|',z,'|'))==%s", omega_val, e_abs_z_val)
  ggplot() +
    geom_histogram(
      data = dt %>% mutate(x = abs(z)),
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = br, fill = col, alpha = 0.25, colour = NA
    ) +
    {if(show_corrected) geom_line(data = df, aes(x = x, y = corrected_fz), linewidth = 0.6, colour = "black")} +
    geom_line(data = df, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    coord_cartesian(xlim = c(-0.1, xmax + 0.1), ylim = c(0, ymax)) +
    labs(x = NULL, y = NULL, title = nm) +
    theme_bw() +
    theme(plot.title = element_text(size = 8),
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    annotate("text", x = 7, y = ymax - 0.055, 
             label = paste("omega ==", omega_val), parse = TRUE, size = 2.5) +
    annotate("text", x = 7, y = ymax - 0.15, 
             label = paste("E(group('|',z,'|')) ==", e_abs_z_val), parse = TRUE, size = 2.5)
    # annotate("text", x = 5, y = ymax - 0.075, label = lab, parse = TRUE, size = 2.5)
}
