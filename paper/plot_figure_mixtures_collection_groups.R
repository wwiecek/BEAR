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

replication_original_panels <- c("SCORE original", "OSC original")

if (!exists("plot_exclude")) {
  plot_exclude <- paper_analysis_exclude
} else {
  plot_exclude <- c(plot_exclude, paper_collection_plot_exclude)
}
plot_exclude <- setdiff(unique(plot_exclude), replication_original_panels)
load_bear_mixture_inputs(exclude = plot_exclude)

set.seed(42)

plots_per_row <- 4
output_path <- "paper/figures/mixtures_plot_collection_groups.pdf"
replication_output_path <- "paper/figures/mixtures_plot_replication_projects.pdf"
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
replication_effort <- paper_groups$replication_effort
collection_group_keys <- setNames(
  paper_dataset_color_group(c(curated_evidence, domain_sample,
                              replication_effort)),
  c(curated_evidence, domain_sample, replication_effort)
)

plotlist <- c(
  category_row(curated_evidence, collection_group_keys),
  category_row(domain_sample, collection_group_keys)
)
expected_group_sizes <- c(curated_evidence = 8L, domain_sample = 6L,
                          replication_effort = 2L)
actual_group_sizes <- c(curated_evidence = length(curated_evidence),
                        domain_sample = length(domain_sample),
                        replication_effort = length(replication_effort))
if (!identical(actual_group_sizes, expected_group_sizes)) {
  stop("Expected collection groups of 8, 6, and 2 datasets, but found: ",
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




# Replication panels -----

replication_panels <- c(
  replication_original_panels[1], "SCORE_replications",
  replication_original_panels[2], "OSC"
)
replication_color_keys <- c(
  "SCORE original" = "original_result",
  "SCORE_replications" = "replication_result",
  "OSC original" = "original_result",
  "OSC" = "replication_result"
)

missing_replication_panels <- setdiff(replication_panels, names(mixtures))
if (length(missing_replication_panels) > 0) {
  stop("Missing replication comparison mixtures for: ",
       paste(missing_replication_panels, collapse = ", "))
}

replication_plotlist <- Map(function(dataset, color_key) {
  dt <- bear_list_thin[[dataset]] %>%
    mutate(group = color_key)
  meanpwr <- psr_table$PoS[psr_table$dataset == dataset]
  if (length(meanpwr) == 0) meanpwr <- NULL

  plot_mixture_v4(
    mixtures[[dataset]],
    dt,
    nm = bear_labels[dataset],
    color_map = group_colors,
    bin_width = 0.245,
    ymax = 0.7,
    meanpwr = if (is.null(meanpwr)) NULL else round(meanpwr, 2),
    show_corrected = TRUE,
    show_fitted = FALSE,
    corrected_colour = group_colors[color_key],
    align_corrected_above_threshold = TRUE
  ) +
    theme(plot.title = element_text(size = panel_title_size),
          axis.text = element_text(size = panel_title_size * 0.75))
}, replication_panels, replication_color_keys[replication_panels])

mixtures_plot_replication_projects <- wrap_plots(replication_plotlist,
                                                 ncol = 4) +
  plot_annotation(caption = "|z|") &
  theme(plot.caption = element_text(hjust = 0.5),
        axis.text = element_text(size = panel_title_size * 0.75))

ggsave(replication_output_path, mixtures_plot_replication_projects,
       height = 5 * a4_text_width_cm / 14,
       width = a4_text_width_cm, units = "cm",
       device = cairo_pdf)



# Omega vs repl -----

omega_repl_collection_groups <- build_omega_repl_data(mixtures) %>%
  filter(dataset %in% paper_selected_datasets) %>%
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
