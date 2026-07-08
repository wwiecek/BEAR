# Changelog

## 2026-07-08

- Clarified that this is a BEAR-internal, LLM-assisted exploratory workflow.
- Made ClinicalTrials.gov predictor mixture fitting opt-in by default so the
  tracked folder stays script-and-doc only unless generated artifacts are
  explicitly requested.
- Documented that subgroup comparisons are one-way summaries and avoid
  interaction-style overlaps.

## 2026-06-20

- Simplified `clinicaltrials_predictors.R` to use only one-way ClinicalTrials.gov
  predictors: effect source, phase, domain, intervention type, and measure.
- Removed interaction-style candidates such as `domain by measure` from the
  descriptive summaries and mixture manifest. The mixture manifest now includes
  all sufficiently large subgroups from the main predictors.
- Omitted predictor categories with fewer than 500 rows and capped each mixture
  fit at 5,000 randomly selected subgroup rows.

## 2026-06-19

- Added `clinicaltrials_predictors.R`, a ClinicalTrials.gov-only exploration
  of z-value summaries by phase, domain, intervention type, effect source,
  and measure.
- The script writes descriptive summary tables by default and includes
  checkpointed, opt-in mixture fitting plus mixture-density and omega-vs-
  replication plots for sufficiently large subgroups.

## 2026-06-04

- Refreshed subgroup fitting to use `fit_mixture(..., mode = "unconstr")` and
  made the saved fit cache drop old fits from other optimisation modes.
- Adjusted `mixture_fits_other_subgroups.pdf` so top-level section heights are
  proportional to each section's number of plot rows.
- Added a simple row-level replication regression script for Cochrane and
  clinicaltrials that controls for specialty, phase, and measure where
  available.

## 2026-05-19

- Added separate fitting and results scripts for subgroup mixture comparisons.
- Covers four categories: measures, Brodeur pre-registration status,
  clinicaltrials.gov phase, and Cochrane specialty.
- Results compare subgroup fits with the existing main fits from `mixtures/`
  using powsignrep summaries, a four-panel omega-vs-replication plot, four
  category tables, and two mixture-fit PDFs.
- Consolidated Cochrane specialty comparison logic into this folder so the old
  `explore/cochrane_subgroups` scripts could be retired.

## 2026-05-21

- Made `fit_models.R` resumable: existing compatible subgroup fits are reused,
  stale manifest entries are dropped, and the fit bundle is saved after each
  completed subgroup fit.
- Aligned the omega-vs-replication comparison with the requested figure
  convention: omega is shown on its raw 0-1 scale with fixed facet scales.
- Documented the expected outputs and confirmed the current manifest includes
  16 measure, 3 pre-registration, 4 clinicaltrials phase, and 12 Cochrane
  specialty subgroup fits at `N >= 1000`.
- Revised the output plots: the Measure panel groups colours by measure
  family, and `mixture_fits_other_subgroups.pdf` allocates section height by
  plot rows so Measure rows use the same vertical space as Pre-registration and
  Phase rows.
