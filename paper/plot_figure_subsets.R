# Rebuild the paper subset mixture figure from explore/subgroups fits.
# Run explore/subgroups/fit_models.R first if the fit cache is missing.

library(tidyverse)
library(patchwork)

source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/fit_density_calc.R")
source("R/psr.R")

fit_file <- "explore/subgroups/fits.rds"
if (!file.exists(fit_file)) {
  stop("Missing ", fit_file, ". Run Rscript --vanilla explore/subgroups/fit_models.R")
}

fit_obj <- readRDS(fit_file)
subgroup_fits <- fit_obj$fits
subgroup_dfs <- fit_obj$dfs
subgroup_manifest <- fit_obj$manifest

plot_subgroup_fit <- function(fit_name, title, ymax) {
  plot_mixture_v4(
    subgroup_fits[[fit_name]],
    subgroup_dfs[[fit_name]],
    ymax = ymax,
    show_corrected = TRUE,
    align_corrected_above_threshold = TRUE
  ) + ggtitle(title)
}

phase_titles <- c(
  phase1 = "Phase 1",
  phase2 = "Phase 2",
  phase3 = "Phase 3",
  phase4 = "Phase 4"
)

phase_panels <- subgroup_manifest %>%
  filter(category == "Phase", subgroup %in% names(phase_titles)) %>%
  mutate(subgroup = factor(subgroup, names(phase_titles))) %>%
  arrange(subgroup) %>%
  transmute(
    fit_name,
    title = paste0("clinicaltrials.gov\n", phase_titles[as.character(subgroup)])
  )

cochrane_panels <- subgroup_manifest %>%
  filter(category == "Cochrane specialty") %>%
  mutate(PoS = map_dbl(fit_name, ~ mean(powsignrep(subgroup_fits[[.x]])$power))) %>%
  arrange(PoS) %>%
  slice(c(1, 2, n() - 1, n())) %>%
  transmute(fit_name, title = paste0("CDSR\n", subgroup))

if (nrow(phase_panels) != 4 || nrow(cochrane_panels) != 4) {
  stop("Expected four phase and four Cochrane panels for subset figure.")
}

ct_row <- wrap_plots(
  pmap(phase_panels, ~ plot_subgroup_fit(..1, ..2, ymax = 0.4)),
  ncol = 4
)

cdsr_row <- wrap_plots(
  pmap(cochrane_panels, ~ plot_subgroup_fit(..1, ..2, ymax = 0.6)),
  ncol = 4
)

p <- wrap_elements(full = ct_row) / wrap_elements(full = cdsr_row)

ggsave(
  "paper/figures/mixtures_plot_subsets.pdf",
  plot = p,
  height = 8.5,
  width = 14,
  units = "cm"
)
