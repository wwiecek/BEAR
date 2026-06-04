# Write one multi-panel selection-mixture plot for the GitHub Pages site.
# Run from the BEAR project root after fitted mixtures are available.

library(tidyverse)
library(ggplot2)
library(patchwork)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

output_path <- "results/selection_mixture_plot.png"
site_output_path <- "site/assets/selection_mixture_plot.png"
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(site_output_path), showWarnings = FALSE, recursive = TRUE)

load_bear_mixture_inputs(exclude = NULL)

initial_order <- c(
  intersect(names(bear_names), names(mixtures)),
  sort(setdiff(names(mixtures), names(bear_names)))
)
mixture_order <- initial_order[order(vapply(mixtures[initial_order],
                                            function(fit) fit$omega[1],
                                            numeric(1)))]

selection_colors <- bear_colors
selection_colors["meta"] <- "#D62728"

missing_inputs <- setdiff(mixture_order, names(bear_list_thin))
if(length(missing_inputs) > 0) {
  stop("Missing bear_list_thin entries: ", paste(missing_inputs, collapse = ", "))
}

draw_selection_panel <- function(dataset) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = bear_classification[dataset])

  title <- dplyr::coalesce(bear_labels[dataset], bear_names[dataset], dataset) %>%
    stringr::str_split_1("\n") %>%
    stringr::str_wrap(width = 20) %>%
    paste(collapse = "\n")
  plot_mixture_v4(
    mixtures[[dataset]],
    dt,
    nm = title,
    color_map = selection_colors,
    bin_width = 0.1225,
    ymax = 0.7,
    annotate = "omega",
    show_corrected = TRUE,
    align_corrected_above_threshold = TRUE
  ) +
    theme(
      plot.title = element_text(size = 8),
      axis.text = element_text(size = 6),
      panel.grid.minor = element_blank()
    )
}

plots <- lapply(mixture_order, draw_selection_panel)
n_panels <- length(plots)
rows <- ceiling(n_panels / 5)
panel_height_cm <- 23 / 7 * 1.2

selection_plot <- patchwork::wrap_plots(plots, ncol = 5) +
  plot_annotation(caption = "|z|") &
  theme(plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = 8 * 0.75))

ggsave(
  filename = output_path,
  plot = selection_plot,
  width = 17.5 * 1.2,
  height = rows * panel_height_cm,
  units = "cm",
  dpi = 300
)

copied <- file.copy(from = output_path, to = site_output_path, overwrite = TRUE)
if(!copied) stop("Failed to copy selection plot to ", site_output_path)

message("Wrote ", n_panels, " selection panels to ", output_path,
        " and ", site_output_path, ".")
