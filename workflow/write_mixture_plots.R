# Write dataset-level mixture plots for the GitHub Pages site.
# Run from the BEAR project root after mixture fits and PSR summaries exist.

library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/site_dataset_config.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

output_dir <- "results/mixture_plots"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

site_mixture_pages <- site_dataset_plot_index("doc/datasets")

load_bear_mixture_inputs(exclude = NULL)

draw_site_mixture_panel <- function(dataset, title) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = bear_classification[dataset])

  plot <- plot_mixture_v4(
    mixtures[[dataset]],
    dt,
    nm = title,
    color_map = bear_colors,
    bin_width = 0.1225,
    ymax = 0.7,
    meanpwr = 0,
    show_corrected = TRUE,
    align_corrected_above_threshold = TRUE
  )
  plot$layers <- plot$layers[-length(plot$layers)]
  plot +
    theme(
      plot.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      panel.grid.minor = element_blank()
    )
}

for(i in seq_len(nrow(site_mixture_pages))) {
  page <- site_mixture_pages[i, ]
  plot <- draw_site_mixture_panel(page$plot_dataset, page$plot_title)
  ggsave(
    filename = file.path(output_dir, page$plot_file),
    plot = plot,
    width = 9,
    height = 5.4,
    dpi = 180
  )
}
