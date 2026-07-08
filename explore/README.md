# Explore Folder

This folder is for exploratory analyses that are useful while developing BEAR
but are not part of the main rebuild path. Scripts here may depend on current
local artifacts under `data/`, `data_raw/`, `mixtures/`, or `results/`.
These folders are typically LLM-developed explorations: treat them as
maintainer-facing scratch workflows, not polished repository or paper
documentation.

For new exploratory work, prefer a small subfolder with:

- a versioned script such as `analysis_name_v1.R`
- a short `README.md` explaining the question, inputs, run command, and outputs
- a `changelog.md` when the folder's purpose or interpretation changes
- outputs under `output/`, `figures/`, or clearly named files in that subfolder

Do not make downstream workflows depend on `explore/` without moving the
relevant code into `process/`, `workflow/`, `R/`, `paper/`, or `site/`.

## Folder Categories

Exploratory folders fall into two broad types:

- BEAR internal: analyses that test, validate, diagnose, or extend BEAR's
  data processing, effect derivation, mixture models, or planned database
  structure. These are tracked by default when the folder is clean and lean.
- Metascience and secondary outputs: analyses that use BEAR or BEAR-derived
  results to make external figures, examples, or substantive claims, without
  primarily improving the database or model workflow. These are ignored by
  default unless there is a specific reason to keep them in git.

### BEAR Internal

- `legacy/`: old mixture and postprocessing scripts, kept only as historical
  context unless an old result needs to be reproduced.
- `loglik/`: older likelihood experiments for mixture fitting; current
  likelihood code lives under `R/`.
- `multiarm_trials/`: screens within-study multiplicity and builds a
  ClinicalTrials.gov shared-comparator table for possible multi-arm or
  correlated-outcome adjustments.
- `omega_pos/`: compares fitted selection parameter `omega` with probability
  of significance and records the ClinicalTrials.gov threshold-censoring issue.
- `replication_mixtures/`: fits BEAR mixture models to original-study
  z-values from replication datasets as a sandbox for replication-specific
  model checks.
- `reports/`: old Rmd report and stitching workflow, likely superseded by the
  paper and site workflows.
- `stan_mixtures/`: old Stan mixture experiments and cached fits, useful only
  if a Bayesian mixture comparison is revived.
- `subgroups/`: subgroup mixture fits and summaries by measure,
  preregistration, phase, Cochrane specialty, and ClinicalTrials.gov
  predictors.
- `z0/`: Cochrane exact-`z = 0` sensitivity analysis for mixture fitting.

### Metascience And Secondary Outputs

- `blog_pubmed_cochrane/`: blog-figure comparison between Barnett/Wren
  biomedical literature results and Cochrane rows.
- `production_function/`: standalone figures illustrating research
  production-function intuition and power-doubling examples.
- `white_paper_graphs/`: publication-style explanatory figures for a popular
  audience white paper or talks.

## Loose Top-Level Scripts

Loose top-level scripts such as `different_k.R`, `euctr_download_ctgov.R`,
`match_studyid.R`, `repro_of_repro.R`, and `test_fake_mixture_data.R` should
either be moved into named subfolders with a README/changelog or removed after
checking whether they still answer a live question.

## Candidates To Promote Or Track

If these analyses become part of BEAR's public results, consider moving or
copying the durable parts out of `explore/`:

- `subgroups/`: promote stable subgroup-fit summaries and plotting code into
  `paper/` or `site/` if subgroup comparisons are reported.
- `omega_pos/clinical_trials_threshold_censoring_issue.md`: keep as a method
  note, or move the final decision into documentation near the likelihood code
  if the censoring issue is resolved.
- `multiarm_trials/`: promote shared-comparator construction only if it becomes
  part of processed ClinicalTrials.gov data or an explicit sensitivity analysis.
- `replication_mixtures/`: consolidate if predictive calibration for
  replication datasets becomes a paper result.
- `white_paper_graphs/` and `production_function/`: move selected final figures
  into `paper/`, `site/`, or a presentation folder if they are used outside
  exploratory drafting.

## Cleanup Notes

Generated binary outputs under `explore/` can be large and are often
rebuildable. Before tracking or deleting them, check whether they are expensive
to regenerate, needed for a manuscript or talk, or the only record of an audit
result. Prefer keeping compact CSV/Markdown summaries over large PDFs, PNGs,
and cached fits unless the binary output is an intentional deliverable.
