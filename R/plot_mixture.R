# Functions for plotting densities of fitted mixtures

# Load fitted mixtures, thinned BEAR inputs, and PoS summaries for plotting.
load_bear_mixture_inputs <- function(exclude = NULL,
                                     mixture_dir = "mixtures",
                                     bear_lists_path = "paper/bear_lists.Rdata",
                                     psr_path = "paper/power_sign_rep.csv",
                                     envir = parent.frame()) {
  mixture_files <- list.files(mixture_dir, pattern = "\\.rds$", full.names = TRUE)
  mixture_names <- sub("\\.rds$", "", basename(mixture_files))
  keep <- if (is.null(exclude)) rep(TRUE, length(mixture_names)) else
    !(mixture_names %in% exclude)

  mixtures <- stats::setNames(
    lapply(mixture_files[keep], readRDS),
    mixture_names[keep]
  )

  bear_env <- new.env(parent = emptyenv())
  load(bear_lists_path, envir = bear_env)

  psr_table <- readr::read_csv(psr_path, show_col_types = FALSE) %>%
    mutate(group = bear_classification[dataset]) %>%
    transmute(dataset, group, PoS = assurance) %>%
    arrange(desc(PoS))

  assign("mixtures", mixtures, envir = envir)
  assign("bear_list_thin", bear_env$bear_list_thin, envir = envir)
  assign("psr_table", psr_table, envir = envir)

  invisible(NULL)
}

# Draw one dataset panel from the shared mixture plotting inputs.
plot_bear_mixture_panel <- function(dataset,
                                    nm = bear_labels[dataset],
                                    color_map = bear_colors,
                                    nbreaks = 25,
                                    ymax = 0.7,
                                    show_corrected = TRUE,
                                    align_corrected_above_threshold = TRUE,
                                    exact_only = TRUE,
                                    ...) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = bear_classification[dataset])

  plot_mixture_v4(
    mixtures[[dataset]],
    dt,
    nm = nm,
    color_map = color_map,
    nbreaks = nbreaks,
    ymax = ymax,
    meanpwr = round(psr_table$PoS[psr_table$dataset == dataset], 2),
    show_corrected = show_corrected,
    align_corrected_above_threshold = align_corrected_above_threshold,
    exact_only = exact_only,
    ...
  )
}

# Current absolute-z mixture plot used by paper/report figure scripts.
plot_mixture_v4 <- function(fit, dt, nm = "", col = "black", color_map = NULL, 
                            xmax = 10, nbreaks = 25, ymax = 0.6, 
                            annotate = "psr", meanpwr = NULL,
                            show_corrected = FALSE,
                            align_corrected_above_threshold = FALSE,
                            exact_only = TRUE) {
  
  # Use color_map if provided and col is default
  if (!is.null(color_map) && col == "black" && "group" %in% names(dt)) {
    col <- color_map[as.character(dt$group[1])]
  }

  if (!"z_operator" %in% names(dt)) dt$z_operator <- "="
  dt <- dt %>% mutate(z_operator = ifelse(is.na(z_operator), "=", z_operator))
  hist_dt <- if (exact_only) {
    dt %>% filter(z_operator == "=")
  } else {
    dt
  }
  if (nrow(hist_dt) == 0) stop("No exact z values available for histogram.")
  
  # calculate the density
  grid_max <- min(500, max(abs(dt$z))) #for stupidly large |z| calculation would crash!
  if(grid_max < xmax) grid_max <- xmax #this makes sure the density gets plotted past max(z) if needed
  den_calc_result <- fit_density_calc_abs(fit, x = seq(0, grid_max, by=0.1))
  df <- den_calc_result$df
  df$corrected_plot <- if (align_corrected_above_threshold) {
    df$aligned_corrected_fz
  } else {
    df$corrected_fz
  }
  bin_width <- 0.245
  br <- seq(0, max(abs(dt$z), xmax) + bin_width, by = bin_width)
  omega_val <- round(fit$omega[1], 2)
  e_abs_z_val <- round(den_calc_result$E_abs_z, 1)
  
  N <- 1e04
  if(annotate == "psr" & is.null(meanpwr)){
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
      data = hist_dt %>% mutate(x = abs(z)),
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = br, fill = col, alpha = 0.25, colour = NA
    ) +
    geom_line(data = df, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    {if(show_corrected) geom_line(data = df, aes(x = x, y = corrected_plot), 
                                  linetype = "22", linewidth = 0.6,
                                  colour = "black")} +
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
