# Explore sensitivity of Cochrane mixture fits to the left-censoring bound
# used for exact z = 0 values in the mixture likelihood.

library(tidyverse)
library(patchwork)

source("R/helpers.R")
source("R/mix.R")
source("R/fit_density_calc.R")
source("R/plot_mixture.R")

dir.create("explore/z0/output/fits", recursive = TRUE, showWarnings = FALSE)
dir.create("explore/z0/output/figures", recursive = TRUE, showWarnings = FALSE)

z0_bounds <- c(0.25, 0.5, 1, 1.96)

bear <- readRDS("BEAR.rds") %>%
  mutate(z_operator = if_else(is.na(z_operator), "=", z_operator))

cochrane <- bear %>%
  filter(dataset == "Cochrane") %>%
  calc_study_weights()

fit_one_bound <- function(z0_bound) {
  fit <- fit_mixture(
    z = cochrane$z,
    operator = cochrane$z_operator,
    weights = cochrane$weights,
    mode = "unconstr",
    z0_bound = z0_bound
  )

  fit$z0_bound <- z0_bound
  saveRDS(
    fit,
    sprintf("explore/z0/output/fits/cochrane_z0_bound_%s.rds",
            gsub("\\.", "p", z0_bound))
  )
  fit
}

fits <- set_names(lapply(z0_bounds, fit_one_bound), as.character(z0_bounds))

fit_summary <- imap_dfr(fits, function(fit, bound) {
  tibble(
    z0_bound = as.numeric(bound),
    n = nrow(cochrane),
    effective_n = sum(cochrane$weights),
    exact_z0 = sum(cochrane$z == 0),
    prop_exact_z0 = mean(cochrane$z == 0),
    prop_abs_z_lt_025 = mean(abs(cochrane$z) < 0.25),
    prop_abs_z_lt_05 = mean(abs(cochrane$z) < 0.5),
    prop_abs_z_lt_1 = mean(abs(cochrane$z) < 1),
    prop_significant = mean(abs(cochrane$z) >= 1.96),
    omega = fit$omega[1],
    p = paste(round(fit$p, 4), collapse = ", "),
    sigma = paste(round(fit$sigma, 4), collapse = ", ")
  )
})

write_csv(fit_summary, "explore/z0/output/cochrane_z0_sensitivity_summary.csv")

summary_lines <- c(
  "Cochrane z = 0 mixture sensitivity",
  "",
  "Input: BEAR.rds rows with dataset == \"Cochrane\".",
  "Exact z = 0 rows are represented as |z| < z0_bound during fitting.",
  "",
  paste(capture.output(print(fit_summary, n = Inf, width = Inf)), collapse = "\n")
)
writeLines(summary_lines, "explore/z0/output/cochrane_z0_sensitivity_summary.txt")

plots <- imap(fits, function(fit, bound) {
  plot_mixture_v4(
    fit,
    cochrane,
    nm = paste0("|z| < ", bound, " for z = 0"),
    col = "#1F77B4",
    xmax = 10,
    ymax = 0.6,
    annotate = "omega",
    z0_bound = as.numeric(bound)
  )
})

comparison_plot <- wrap_plots(plots, nrow = 1) +
  plot_annotation(title = "Cochrane mixture sensitivity to exact-zero handling")

ggsave(
  "explore/z0/output/figures/cochrane_z0_sensitivity.pdf",
  comparison_plot,
  width = 11,
  height = 3.2,
  units = "in",
  device = cairo_pdf
)

dataset_zero_summary <- bear %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    exact_z0 = sum(z == 0, na.rm = TRUE),
    prop_exact_z0 = mean(z == 0, na.rm = TRUE),
    abs_z_lt_025 = sum(abs(z) < 0.25, na.rm = TRUE),
    prop_abs_z_lt_025 = mean(abs(z) < 0.25, na.rm = TRUE),
    abs_z_lt_05 = sum(abs(z) < 0.5, na.rm = TRUE),
    prop_abs_z_lt_05 = mean(abs(z) < 0.5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_exact_z0), desc(prop_abs_z_lt_025))

write_csv(dataset_zero_summary, "explore/z0/output/dataset_zero_summary.csv")

cat("\nTop datasets by exact z = 0 proportion:\n")
print(head(dataset_zero_summary, 12), n = 12)

cat("\nTop datasets by |z| < 0.25 proportion:\n")
print(
  dataset_zero_summary %>%
    arrange(desc(prop_abs_z_lt_025), desc(prop_exact_z0)) %>%
    head(12),
  n = 12
)
