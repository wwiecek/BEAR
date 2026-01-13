# Functions for plotting densities of fitted mixtures

plot_mixture_v3 <- function(fit, dt, nm = "", col = "black", xmax = 10, nbreaks = 25, ymax = 0.6) {
  # calculate the density
  den_calc  <- fit_density_calc_abs(fit, x = seq(0, min(500, max(abs(dt$z))), by=0.1))
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
                            xmax = 10, nbreaks = 25, ymax = 0.6, 
                            annotate = "psr", 
                            show_corrected = FALSE) {
  
  # Use color_map if provided and col is default
  if (!is.null(color_map) && col == "black" && "group" %in% names(dt)) {
    col <- color_map[as.character(dt$group[1])]
  }
  
  # calculate the density
  grid_max <- min(500, max(abs(dt$z))) #for stupidly large |z| calculation would crash!
  if(grid_max < xmax) grid_max <- xmax #this makes sure the density gets plotted past max(z) if needed
  den_calc_result <- fit_density_calc_abs(fit, x = seq(0, grid_max, by=0.1))
  df <- den_calc_result$df
  br <- c(seq(0, xmax, length = nbreaks), max(abs(dt$z)))
  omega_val <- round(fit$omega[1], 2)
  e_abs_z_val <- round(den_calc_result$E_abs_z, 1)
  
  N <- 1e04
  if(annotate == "psr"){
    snr     <- rmix(N, p = fit$p, m = fit$m, s = fit$sigma_SNR)
    z       <- snr + rnorm(N)
    power   <- (1 - pnorm(1.96, snr, 1)) + pnorm(-1.96, snr, 1)
    meanpwr <- round(mean(power), 2)
  }
  # lab <- sprintf("atop(omega==%s, E(group('|',z,'|'))==%s)", omega_val, e_abs_z_val)
  # lab <- sprintf("atop(omega==%s, scriptstyle(E(group('|',z,'|'))==%s))", omega_val, e_abs_z_val)
  # lab <- sprintf("omega==%s~~E(group('|',z,'|'))==%s", omega_val, e_abs_z_val)
  ggplot() +
    geom_histogram(
      data = dt %>% mutate(x = abs(z)),
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = br, fill = col, alpha = 0.25, colour = NA
    ) +
    {if(show_corrected) geom_line(data = df, aes(x = x, y = corrected_fz), 
                                  lty = "dashed", linewidth = 0.6, colour = "black")} +
    geom_line(data = df, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    coord_cartesian(xlim = c(-0.1, xmax + 0.1), ylim = c(0, ymax)) +
    labs(x = NULL, y = NULL, title = nm) +
    theme_bw() +
    theme(plot.title = element_text(size = 8),
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    {if(annotate != "psr") annotate("text", x = 7, y = ymax - 0.055, 
             label = paste("omega ==", omega_val), parse = TRUE, size = 2.5) } +
    {if(annotate != "psr") annotate("text", x = 7, y = ymax - 0.15, 
             label = paste("E(group('|',z,'|')) ==", e_abs_z_val), parse = TRUE, size = 2.5) } +
    {if(annotate == "psr") annotate("text", x = 7, y = ymax - 0.055, 
                                    label = paste("bar(PoS) ==", meanpwr), 
                                    parse = TRUE, size = 2.5) }
  
    # annotate("text", x = 5, y = ymax - 0.075, label = lab, parse = TRUE, size = 2.5)
}



# Legacy functions from Erik (edited by WW), no longer in use

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

# Erik's function that used to do analysis within Rmd, I am now suggesting we
# precalculate this before running Rmd reports
fit_and_plot = function(z,truncated,k=4,weights){
  fit <- fit_mixture(z,truncated,k=4,weights)
  ggp <- plot_mixture(fit, z, weights)
  print(ggp)
  return(fit)
}
