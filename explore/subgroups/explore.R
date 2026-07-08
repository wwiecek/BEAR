# Compare subgroup mixture fits with main dataset fits.
# Run fit_models.R first only when subgroup fits need refreshing.

library(tidyverse)
library(patchwork)
library(ggrepel)
library(scales)

source("R/helpers.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/fit_density_calc.R")
source("R/psr.R")

fit_file <- "explore/subgroups/fits.rds"
out_dir <- "explore/subgroups/output"
main_fit_dir <- "mixtures"
category_levels <- c("Measure", "Pre-registration", "Phase",
                     "Cochrane specialty")
measure_colour_values <- c(
  "Full dataset" = "black",
  "Continuous: SMD / mean difference" = "#1B9E77",
  "Continuous: correlation" = "#66A61E",
  "Binary or hazard ratio" = "#D95F02",
  "Ratio scale" = "#7570B3",
  "Other / uncommon" = "#6A3D9A",
  "Other subgroup" = "#377EB8"
)

set.seed(20260519)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

prepare_main_dfs <- function(datasets) {
  readRDS("BEAR.rds") %>%
    mutate(z_operator = if_else(is.na(z_operator), "=", z_operator)) %>%
    filter(dataset %in% datasets, !is.na(z)) %>%
    split(.$dataset) %>%
    lapply(function(df) df %>% calc_study_weights() %>% thin_df())
}

load_main_fits <- function(datasets) {
  setNames(
    lapply(datasets, function(dataset) {
      readRDS(file.path(main_fit_dir, paste0(dataset, ".rds")))
    }),
    datasets
  )
}

fit_summary <- function(fit, df, dataset, label, category, row_type) {
  psr <- powsignrep(fit)
  psr_signif <- powsignrep(fit, z_star = 1.96)
  omega <- fit$omega[1]

  tibble(
    category = category,
    dataset = dataset,
    label = label,
    row_type = row_type,
    N = nrow(df),
    omega = omega,
    omega_odds = omega / (1 - omega),
    power = mean(psr$power),
    sign = mean(psr$sgn),
    replication = mean(psr$rep),
    repl_signif = mean(psr_signif$rep)
  )
}

make_plot <- function(fit, df, label, row_type) {
  plot_mixture_v4(
    fit,
    df %>% mutate(group = row_type),
    nm = label,
    color_map = c(main = "black", subgroup = "#377EB8"),
    ymax = 0.6,
    show_corrected = TRUE,
    align_corrected_above_threshold = TRUE
  )
}

measure_colour_group <- function(category, label, row_type) {
  case_when(
    row_type == "main" ~ "Full dataset",
    category != "Measure" ~ "Other subgroup",
    label %in% c("SMD", "Mean Difference") ~
      "Continuous: SMD / mean difference",
    label == "Zr" ~ "Continuous: correlation",
    label %in% c("probit", "Hazard Ratio", "Odds Ratio",
                 "Risk Difference") ~ "Binary or hazard ratio",
    label %in% c("lnRR", "Geometric Ratio", "Other Ratio") ~ "Ratio scale",
    TRUE ~ "Other / uncommon"
  )
}

section_header <- function(label) {
  ggplot() +
    annotate("text", x = 0, y = 0.5, label = label, hjust = 0,
             fontface = "bold", size = 4) +
    xlim(0, 1) +
    theme_void()
}

write_category_table <- function(summary_table, category) {
  category_safe <- str_to_lower(str_replace_all(category, "[^A-Za-z0-9]+", "_"))
  table <- summary_table %>%
    filter(category == .env$category) %>%
    select(dataset, label, row_type, N, omega, omega_odds, power, sign,
           replication, repl_signif)

  write_csv(table, file.path(out_dir, paste0(category_safe, "_summary.csv")))

  lines <- c(
    paste(category, "mixture summaries"),
    paste("Generated:", Sys.time()),
    "",
    capture.output(print(table, n = Inf))
  )
  writeLines(lines, file.path(out_dir, paste0(category_safe, "_summary.txt")))
}

fit_obj <- readRDS(fit_file)

subgroup_fits <- fit_obj$fits
subgroup_dfs <- fit_obj$dfs
subgroup_manifest <- fit_obj$manifest %>%
  mutate(
    category = factor(category, category_levels),
    row_type = "subgroup",
    label = subgroup
  )

main_datasets <- subgroup_manifest %>%
  distinct(dataset) %>%
  pull(dataset)

main_fits <- load_main_fits(main_datasets)
main_dfs <- prepare_main_dfs(main_datasets)

main_manifest <- subgroup_manifest %>%
  distinct(category, dataset) %>%
  mutate(
    fit_name = dataset,
    subgroup = NA_character_,
    subgroup_variable = NA_character_,
    N = map_int(dataset, ~ nrow(main_dfs[[.x]])),
    row_type = "main",
    label = "Full dataset"
  )

all_fits <- c(main_fits, subgroup_fits)
all_dfs <- c(main_dfs, subgroup_dfs)
all_manifest <- bind_rows(main_manifest, subgroup_manifest) %>%
  arrange(category, dataset, row_type != "main", desc(N)) %>%
  mutate(label = as.character(label))

all_summary <- pmap_dfr(
  all_manifest %>% select(fit_name, dataset, label, category, row_type),
  function(fit_name, dataset, label, category, row_type) {
    fit_summary(
      all_fits[[fit_name]],
      all_dfs[[fit_name]],
      dataset,
      label,
      as.character(category),
      row_type
    )
  }
) %>%
  arrange(factor(category, category_levels), dataset,
          row_type != "main", desc(N)) %>%
  mutate(colour_group = measure_colour_group(category, label, row_type))

summary_table <- all_summary %>%
  mutate(
    omega = round(omega, 3),
    omega_odds = round(omega_odds, 3),
    across(c(power, sign, replication, repl_signif), ~ round(.x, 3))
  )

write_csv(summary_table, file.path(out_dir, "all_subgroup_summaries.csv"))
walk(category_levels, ~ write_category_table(summary_table, .x))

omega_plot <- ggplot(
  all_summary,
  aes(omega, repl_signif, colour = colour_group, shape = row_type)
) +
  geom_point(size = 2.4) +
  geom_text_repel(
    aes(label = paste(dataset, label, sep = ": ")),
    size = 2.3,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 0.18,
    point.padding = 0.14,
    segment.colour = "grey70",
    segment.size = 0.22,
    seed = 20260519
  ) +
  facet_wrap(~ category, scales = "fixed", ncol = 2) +
  scale_colour_manual(values = measure_colour_values) +
  scale_shape_manual(values = c(main = 16, subgroup = 1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = expression(omega),
    y = "Pr(replication if |z| > 1.96)",
    colour = NULL,
    shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(8, 12, 8, 8)
  )

ggsave(
  file.path(out_dir, "omega_vs_repl_4_panel.pdf"),
  plot = omega_plot,
  width = 24,
  height = 18,
  units = "cm"
)

plot_manifest <- all_manifest %>%
  mutate(plot_label = paste(dataset, label, sep = ": "))

cochrane_specialty_plots <- plot_manifest %>%
  filter(category == "Cochrane specialty") %>%
  pmap(function(category, dataset, subgroup, N, fit_name, subgroup_variable,
                row_type, label, plot_label, ...) {
    make_plot(all_fits[[fit_name]], all_dfs[[fit_name]], plot_label,
              row_type)
  })

ggsave(
  file.path(out_dir, "mixture_fits_cochrane_specialties.pdf"),
  plot = wrap_plots(cochrane_specialty_plots, ncol = 3),
  width = 18,
  height = max(12, 4 * ceiling(length(cochrane_specialty_plots) / 3)),
  units = "cm",
  limitsize = FALSE
)

other_categories <- c("Measure", "Pre-registration", "Phase")
plot_row_height <- 4
section_header_height <- 0.6
other_section_sizes <- plot_manifest %>%
  filter(category %in% other_categories) %>%
  count(category) %>%
  mutate(
    plot_rows = ceiling(n / 3),
    section_height = section_header_height + plot_row_height * plot_rows
  )
other_sections <- map(
  other_categories,
  function(category_name) {
    category_plots <- plot_manifest %>%
      filter(category == category_name) %>%
      pmap(function(category, dataset, subgroup, N, fit_name, subgroup_variable,
                    row_type, label, plot_label, ...) {
        make_plot(all_fits[[fit_name]], all_dfs[[fit_name]], plot_label,
                  row_type)
      })

    section_header(category_name) /
      wrap_plots(category_plots, ncol = 3) +
      plot_layout(
        heights = c(
          section_header_height,
          plot_row_height * ceiling(length(category_plots) / 3)
        )
      )
  }
)
other_section_heights <- other_section_sizes %>%
  mutate(category = factor(category, other_categories)) %>%
  arrange(category) %>%
  pull(section_height)

ggsave(
  file.path(out_dir, "mixture_fits_other_subgroups.pdf"),
  plot = wrap_plots(other_sections, ncol = 1,
                    heights = other_section_heights),
  width = 18,
  height = sum(other_section_heights),
  units = "cm",
  limitsize = FALSE
)
