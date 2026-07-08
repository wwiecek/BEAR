# Subgroup Mixture Fits

This is a BEAR-internal, maintainer-facing exploration. It was developed with
LLM assistance and should be treated as scratch analysis code, not as polished
paper documentation.

Explores whether mixture fits differ across four subgroup categories:

- Cochrane, clinicaltrials.gov, CostelloFox, and Yang by effect-size measure
- Brodeur by pre-registration status
- clinicaltrials.gov by phase
- Cochrane by specialty

Subgroups are included only when they have at least 1,000 non-missing z rows.
The categories are deliberately one-way summaries. The scripts do not fit
interaction-style subgroup overlaps, and `explore.R` adds each main dataset fit
once per comparison family so the full-dataset reference is visible without
duplicating subgroup fits.

Run order:

1. `Rscript explore/subgroups/fit_models.R`
2. `Rscript explore/subgroups/explore.R`

For the richer ClinicalTrials.gov-only predictor exploration, run:

`Rscript explore/subgroups/clinicaltrials_predictors.R`

That script writes descriptive summaries by phase, domain, intervention type,
effect source, and measure, omitting categories with fewer than 500 rows.
Mixture fitting is present
but disabled by default; set `run_mixtures <- TRUE` in the script when those
fits should be refreshed. Subgroup fits use at most 3,000 randomly selected
rows per category.

`fit_models.R` is checkpointed: it loads `explore/subgroups/fits.rds` if it
exists, skips compatible saved subgroup fits, drops fits whose manifest rows
changed, and saves after each newly completed fit.

The results script adds the full dataset-level fits from `mixtures/` to the
omega-vs-replication plot and category summary tables. It writes:

- `output/omega_vs_repl_4_panel.pdf`, a 2x2 raw-scale omega plot.
- `output/mixture_fits_other_subgroups.pdf`, for measure, pre-registration,
  and phase comparisons.
- `output/mixture_fits_cochrane_specialties.pdf`, for Cochrane specialties.
- one CSV and text summary per comparison category.

Generated outputs and fit caches under `output/` are ignored. Re-run the scripts
from the project root when those artifacts are needed locally.
