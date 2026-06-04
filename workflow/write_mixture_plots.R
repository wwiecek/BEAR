# Write dataset-level mixture plots for the GitHub Pages site.
# Run from the BEAR project root after mixture fits and PSR summaries exist.

library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R")
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

output_dir <- "results/mixture_plots"
site_output_dir <- "site/assets/mixture_plots"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(site_output_dir, showWarnings = FALSE, recursive = TRUE)

site_mixture_pages <- tibble::tribble(
  ~slug, ~dataset, ~title,
  "arelbundock", "ArelBundock", "Arel-Bundock et al 2022",
  "askarov", "Askarov", "Askarov et al 2023",
  "barnettwren", "BarnettWren", "Barnett and Wren",
  "bartos", "Bartos", "Bartos et al 2025",
  "brodeur", "Brodeur", "Brodeur et al 2024",
  "chavalarias", "Chavalarias", "Chavalarias et al 2016",
  "clinicaltrials-gov", "ctgov_euctr", "Clinical-trials registries",
  "cochrane", "Cochrane", "Cochrane Database of Systematic Reviews",
  "costellofox-yang", "CostelloFox", "Costello and Fox; Yang et al",
  "euctr", "ctgov_euctr", "Clinical-trials registries",
  "head", "Head", "Head et al 2015",
  "jagerleek", "JagerLeek", "Jager and Leek",
  "lang", "Lang", "Lang 2025",
  "manylabs2", "ManyLabs2", "Many Labs 2",
  "metapsy", "Metapsy", "Metapsy",
  "osc", "OSC", "Open Science Collaboration 2015",
  "psymetadata-nuijten", "psymetadata", "psymetadata and Nuijten et al",
  "score", "SCORE_claims", "SCORE",
  "sladekova", "Sladekova", "Sladekova et al 2023",
  "szucs", "Szucs", "Szucs and Ioannidis 2017",
  "wwc", "WWC", "What Works Clearinghouse"
)

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
  plot <- draw_site_mixture_panel(page$dataset, page$title)
  ggsave(
    filename = file.path(output_dir, paste0(page$slug, ".png")),
    plot = plot,
    width = 9,
    height = 5.4,
    dpi = 180
  )
  file.copy(
    from = file.path(output_dir, paste0(page$slug, ".png")),
    to = file.path(site_output_dir, paste0(page$slug, ".png")),
    overwrite = TRUE
  )
}
