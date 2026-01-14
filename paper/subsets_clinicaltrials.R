library(tidyverse)
library(tictoc)
library(digest)
library(gridExtra)
library(patchwork)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")

df_ct_raw <- readRDS("data/clinicaltrialsgov.rds")
df_ct <- df_ct_raw %>% 
  group_by(nct_id) %>% filter(n() < 20) %>% ungroup %>% 
  transmute(studyid = nct_id,
            study_type = tolower(study_type), 
            allocation = tolower(allocation),
            primary_purpose = tolower(primary_purpose), 
            masking = tolower(masking),
            year = year,
            phase = tolower(phase),
            method = "RCT",
            measure = measure_class,
            z = z,
            b = effect,
            se = se,
            ss = enrollment,
            z_operator = "=")


# df_ct_raw %>% 
#   filter(phase == "PHASE3") %>% 
#   filter(enrollment > 1000) %>% 
#   filter(primary_purpose == "TREATMENT") %>% 
#   pull(brief_title) %>% unique

ct_subsets <- list(
  obs = df_ct %>% filter(study_type == "observational"),
  ph1 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase1"),
  ph2 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase2"),
  ph3 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3"),
  ph4 = df_ct %>% filter(primary_purpose == "treatment", phase == "phase4"),
  ph3lrg = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss > 1000),
  ph3sml = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss < 1000),
  ph3best = df_ct %>% filter(primary_purpose == "treatment", phase == "phase3", ss > 2000, masking != "none")
) %>% 
  lapply(function(x) x %>% group_by(studyid) %>% mutate(weights = 1/n()) %>% ungroup())

ct_fits <- lapply(ct_subsets, fit_mixture_df)
# ct_fits$ph3best <- fit_mixture_df(ct_subsets$ph3best)

save(ct_subsets, ct_fits, file="paper/results/clinicaltrials_subset_fits.Rdata")



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
for(nm in names(ct_fits)) {
  ct_pl[[nm]] <- plot_mixture_v4(ct_fits[[nm]], ct_subsets[[nm]], 
                                 nbreaks = 40,
                                 ymax = 0.4) + ggtitle(ct_names[[nm]])}
wrap_plots(ct_pl)
ggsave("paper/figures/mixtures_plot_clintrials.pdf", height = 14, width = 14, units = "cm")

# But for the publication I want to only have four phases
wrap_plots(ct_pl[2:5], ncol = 4)
ggsave("paper/figures/mixtures_plot_clintrials_4.pdf", height = 5, width = 16, units = "cm")

save(ct_subsets, ct_fits, ct_pl, file="paper/results/clinicaltrials_subset_fits.Rdata")

