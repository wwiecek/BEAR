line_df <- function(fit, x = grid_x, c_star = 1.96) {
  omega <- fit$omega[1]
  B1    <- 2 * pmix(-c_star, p = fit$p, m = fit$m, s = fit$sigma)
  B2    <- 1 - B1
  tibble(
    x  = x,
    fz = 2 * (omega * (x < c_star) + (x >= c_star)) *
      dmix(x, p = fit$p, m = fit$m, s = fit$sigma) / (B1 + omega * B2)
  )
}

plot_mixture_v3 <- function(fit, dt, nm = "", col = "black", xmax = 10, nbreaks = 25, ymax = 0.6) {
  df  <- line_df(fit, x = seq(0,xmax,by=0.01))
  # br <- pretty(range(abs(dt$z), na.rm = TRUE), n = nbreaks)
  br <- c(seq(0, xmax, length = nbreaks), max(abs(dt$z)))
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
    theme(plot.title = element_text(size = 9),
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    annotate("text", 
             x = 7, y = 0.55,   
             label = paste("omega ==", round(fit$omega[1], 2)),
             parse = TRUE,
             size = 3.5)
}

plot_mixture_many <- function(fit, dt, nm = "", col = "black") {
  df  <- line_df(fit, x = seq(0,20,by=0.01))
  ggplot() +
    geom_histogram(
      data = dt %>% mutate(x = abs(z)),
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = seq(0, 20, 0.4),
      fill = col, alpha = 0.25, colour = NA
    ) +
    geom_line(data = df, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    coord_cartesian(xlim = c(-0.1, 20.1), ylim = c(0, 0.6)) +
    labs(x = NULL, y = NULL, title = nm) +
    theme_bw() +
    theme(plot.title = element_text(size = 9),
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    annotate("text", 
             x = 7, y = 0.55,   
             label = paste("omega ==", round(fit$omega[1], 2)),
             parse = TRUE,
             size = 3.5)
}
