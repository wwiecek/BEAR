library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
source("R/psr.R") # for powsignrep()
source("R/fit_density_calc.R")
source("R/plot_mixture.R")
source("R/plot_omega_repl.R")
source("R/paper_selection.R")

if (!exists("plot_exclude")) {
  plot_exclude <- paper_analysis_exclude
} else {
  plot_exclude <- c(plot_exclude, paper_collection_plot_exclude)
}
plot_exclude <- unique(plot_exclude)
load_bear_mixture_inputs(exclude = plot_exclude)

set.seed(42)

plots_per_row <- 4
output_path <- "paper/figures/mixtures_plot_collection_groups.pdf"
omega_repl_output_path <- "paper/figures/omega_vs_repl_collection_groups.pdf"
panel_title_size <- 7.2
a4_text_width_cm <- 16.5
group_colors <- c(
  paper_collection_group_colors,
  "pubmed_scrape" = "#4DAF4A"
)

available_datasets <- names(mixtures)
classified_datasets <- intersect(names(bear_dataset_classes$replication),
                                 available_datasets)

draw_panel <- function(dataset, color_key) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = color_key)

  plot_mixture_v4(
    mixtures[[dataset]],
    dt,
    nm = bear_labels[dataset],
    color_map = group_colors,
    bin_width = 0.245,
    ymax = 0.7,
    meanpwr = round(psr_table$PoS[psr_table$dataset == dataset], 2),
    show_corrected = TRUE,
    align_corrected_above_threshold = TRUE
  ) +
    theme(plot.title = element_text(size = panel_title_size),
          axis.text = element_text(size = panel_title_size * 0.75))
}

category_row <- function(datasets, color_keys) {
  datasets <- order_paper_datasets(datasets, psr_table)
  plots <- Map(draw_panel, datasets, color_keys[datasets])
  n_spacers <- (plots_per_row - length(plots) %% plots_per_row) %% plots_per_row
  c(plots, rep(list(plot_spacer()), n_spacers))
}

paper_groups <- lapply(paper_dataset_groups, intersect, classified_datasets)
curated_evidence <- paper_groups$curated_evidence
domain_sample <- paper_groups$domain_sample
collection_group_keys <- setNames(
  paper_dataset_color_group(c(curated_evidence, domain_sample)),
  c(curated_evidence, domain_sample)
)

plotlist <- c(
  category_row(curated_evidence, collection_group_keys),
  category_row(domain_sample, collection_group_keys)
)
expected_group_sizes <- c(curated_evidence = 8L, domain_sample = 8L)
actual_group_sizes <- c(curated_evidence = length(curated_evidence),
                        domain_sample = length(domain_sample))
if (!identical(actual_group_sizes, expected_group_sizes)) {
  stop("Expected two collection groups of 8 datasets, but found: ",
       paste(names(actual_group_sizes), actual_group_sizes,
             sep = " = ", collapse = ", "))
}
rows <- length(plotlist) / plots_per_row

mixtures_plot_collection_groups <- wrap_plots(plotlist, ncol = plots_per_row) +
  plot_annotation(caption = "|z|") &
  theme(plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = panel_title_size * 0.75))

ggsave(output_path, mixtures_plot_collection_groups,
       height = rows * 4 * a4_text_width_cm / 14,
       width = a4_text_width_cm, units = "cm",
       device = cairo_pdf)

omega_repl_collection_groups <- build_omega_repl_data(mixtures) %>%
  mutate(colour_group = unname(collection_group_keys[dataset]))

missing_colour_groups <- omega_repl_collection_groups %>%
  filter(is.na(colour_group)) %>%
  pull(dataset)
if(length(missing_colour_groups) > 0) {
  stop("Missing collection colour groups for: ",
       paste(missing_colour_groups, collapse = ", "))
}

omega_vs_repl_collection_groups <- plot_omega_repl(
  omega_repl_collection_groups,
  omega_scale = "raw",
  colour_var = "colour_group",
  colour_values = group_colors
)

ggsave(omega_repl_output_path, omega_vs_repl_collection_groups,
       width = 18, height = 13, units = "cm")
