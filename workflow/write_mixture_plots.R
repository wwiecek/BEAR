# Write dataset-level mixture plots for the GitHub Pages site.
# Run from the BEAR project root after mixture fits and PSR summaries exist.

library(tidyverse)
library(ggplot2)
library(digest)

source("R/settings.R")
source("R/site_dataset_config.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

output_dir <- "results/mixture_plots"
hash_path <- "results/mixture_plots_hash.rds"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

site_mixture_pages <- site_dataset_plot_index("doc/datasets")

load_bear_mixture_inputs(exclude = NULL)

previous_hash <- if(file.exists(hash_path)) readRDS(hash_path) else list()
plot_hash <- list()

mixture_plot_hash <- function(dataset, title, plot_file) {
  dt <- bear_list_thin[[dataset]]
  digest(list(
    plot_version = 1,
    dataset = dataset,
    title = title,
    plot_file = plot_file,
    workflow_classification = bear_dataset_classes$workflow_classification[dataset],
    mixture = mixtures[[dataset]],
    fitting_data = dt[c("z", "z_operator", "weights")]
  ))
}

draw_site_mixture_panel <- function(dataset, title) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = bear_dataset_classes$workflow_classification[dataset])

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
  output_path <- file.path(output_dir, page$plot_file)
  current_hash <- mixture_plot_hash(page$plot_dataset, page$plot_title,
                                    page$plot_file)
  plot_hash[[page$plot_file]] <- current_hash
  if(file.exists(output_path) &&
     identical(previous_hash[[page$plot_file]], current_hash)) {
    next
  }

  plot <- draw_site_mixture_panel(page$plot_dataset, page$plot_title)
  ggsave(
    filename = output_path,
    plot = plot,
    width = 9,
    height = 5.4,
    dpi = 180
  )
}

saveRDS(plot_hash, hash_path)
