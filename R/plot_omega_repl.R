# Build and plot the relationship between fitted omega and replication
# probability among significant original results.

build_omega_repl_data <- function(mixtures = load_all_mixtures(),
                                  repl_path = "paper/power_sign_rep.csv",
                                  seed = 20260518) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if(had_seed)
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if(had_seed)
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)

  paper_repl <- read_csv(repl_path, show_col_types = FALSE) %>%
    select(dataset, repl_signif)

  omega_repl <- tibble(dataset = names(mixtures)) %>%
    mutate(
      label = recode(dataset, !!!bear_names, .default = dataset),
      class = recode(dataset, !!!bear_dataset_classes$workflow_classification,
                     .default = "other"),
      in_paper_set = !dataset %in% paper_do_not_include,
      omega = map_dbl(dataset, ~ mixtures[[.x]]$omega[1]),
      omega_plot = pmin(pmax(omega, 0), 1),
      omega_odds = omega / (1 - omega),
      omega_odds_plot = if_else(omega >= 1 - 1e-6, 100, omega_odds)
    ) %>%
    left_join(paper_repl, by = "dataset")

  missing_repl <- omega_repl %>%
    filter(is.na(repl_signif)) %>%
    mutate(repl_signif = map_dbl(
      dataset,
      ~ mean(powsignrep(mixtures[[.x]], z_star = 1.959)$rep)
    )) %>%
    select(dataset, repl_signif)

  omega_repl <- omega_repl %>%
    select(-repl_signif) %>%
    left_join(bind_rows(
      filter(paper_repl, !is.na(repl_signif)),
      missing_repl
    ), by = "dataset")

  if(nrow(omega_repl) != length(mixtures) ||
     anyDuplicated(omega_repl$dataset) > 0) {
    stop("Omega-replication data must have one row per mixture.")
  }

  omega_repl
}

omega_repl_colours <- function() {
  plot_colors <- bear_colors
  plot_colors["meta"] <- "#E41A1C"
  c(plot_colors, other = "#6B7280")
}

plot_omega_repl <- function(data, omega_scale = c("raw", "odds"),
                            colour_var = "class",
                            colour_values = omega_repl_colours(),
                            label_var = "label") {
  omega_scale <- match.arg(omega_scale)
  missing_cols <- setdiff(c("repl_signif", "in_paper_set", colour_var,
                            label_var), names(data))
  if(length(missing_cols) > 0)
    stop("Missing columns in omega-replication data: ",
         paste(missing_cols, collapse = ", "))

  missing_colours <- setdiff(unique(data[[colour_var]]), names(colour_values))
  missing_colours <- missing_colours[!is.na(missing_colours)]
  if(length(missing_colours) > 0)
    stop("Missing colour values for: ", paste(missing_colours, collapse = ", "))

  if(omega_scale == "raw") {
    x_var <- "omega_plot"
    x_scale <- scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                                  limits = c(0, 1))
    x_label <- expression(omega)
  } else {
    x_var <- "omega_odds_plot"
    x_scale <- scale_x_log10(
      breaks = c(0.03, 0.1, 0.3, 1, 3, 10, 30, 100),
      labels = c("0.03", "0.1", "0.3", "1", "3", "10", "30", "Inf")
    )
    x_label <- expression("Odds corresponding to " * omega)
  }

  ggplot(data, aes(.data[[x_var]], repl_signif)) +
    geom_point(aes(colour = .data[[colour_var]], shape = in_paper_set),
               size = 2.8) +
    ggrepel::geom_text_repel(
      aes(label = .data[[label_var]]),
      size = 3,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.22,
      point.padding = 0.18,
      segment.colour = "grey70",
      segment.size = 0.25,
      seed = 20260518
    ) +
    scale_colour_manual(values = colour_values) +
    scale_shape_manual(
      values = c(`TRUE` = 16, `FALSE` = 1),
      labels = c(`TRUE` = "Paper set", `FALSE` = "Excluded from paper set")
    ) +
    x_scale +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = x_label,
      y = "Pr(replication if |z| > 1.96)",
      colour = NULL,
      shape = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
      legend.position = "top",
      axis.title = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
}
