library(tidyverse)
library(patchwork)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/fit_density_calc.R")
source("R/psr.R")

# Cochrane (CDSR) subsets
load("paper/results/cdsr_subset_fits.Rdata")

cdsr_pl <- list()
for (nm in names(cdsr_fits)) {
  cdsr_pl[[nm]] <- plot_mixture_v4(
    cdsr_fits[[nm]],
    cdsr_sub[[nm]],
    nbreaks = 40,
    ymax = 0.6
  ) + ggtitle(paste0("CDSR\n", nm))
}

cdsr_pos <- lapply(cdsr_fits, function(x) mean(powsignrep(x)$power)) %>% unlist()
cdsr_choose <- names(sort(cdsr_pos)[c(1,2,length(cdsr_pos)-1,length(cdsr_pos))])

# clinicaltrials.gov subsets
load("paper/results/clinicaltrials_subset_fits.Rdata")
ct_names <- c(
  obs = "Observational",
  ph1 = "Phase 1",
  ph2 = "Phase 2",
  ph3 = "Phase 3",
  ph4 = "Phase 4",
  ph3lrg = "Phase 3, N > 1,000",
  ph3sml = "Phase 3, N < 1,000",
  ph3best = "Ph3, N > 2,000, blinded"
)

ct_pl <- list()
for (nm in names(ct_fits)) {
  ct_pl[[nm]] <- plot_mixture_v4(
    ct_fits[[nm]],
    ct_subsets[[nm]],
    nbreaks = 40,
    ymax = 0.4
  ) + ggtitle(paste0("clinicaltrials.gov\n", ct_names[[nm]]))
}

ct_row <- wrap_plots(ct_pl[c(2,3,4,8)], ncol = 4)

cdsr_row <- wrap_plots(cdsr_pl[cdsr_choose], ncol = 4)
p <- wrap_elements(full = ct_row) / wrap_elements(full = cdsr_row)

ggsave(
  "paper/figures/mixtures_plot_subsets.pdf",
  plot = p,
  height = 8.5,
  width = 14,
  units = "cm"
)




