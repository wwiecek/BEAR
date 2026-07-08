# Multiarm Trial Screening

This is a BEAR-internal, maintainer-facing exploration. It was developed with
LLM assistance and should be treated as scratch analysis code, not as polished
paper documentation.

This folder screens BEAR datasets for within-study multiplicity that may
reflect multi-arm trials or multiple outcomes per trial.

Run from the project root with:

```r
Rscript explore/multiarm_trials/screen_multiarm_candidates_v1.R
```

Outputs:

- `bear_study_multiplicity_v1.csv`: broad ranking of current BEAR datasets
  by rows per `studyid`
- `candidate_screen_v1.csv`: targeted screen of datasets most relevant to
  multi-arm or multi-outcome work
- `screen_notes_v1.md`: compact recommendations and caveats

To build the cleaned clinicaltrials.gov shared-comparator dataset, run:

```r
Rscript explore/multiarm_trials/build_ctgov_shared_comparator_v1.R
```

Additional outputs:

- `ctgov_multiarm_shared_comparator_v1.rds`: BEAR-like long table of
  pairwise active-versus-shared-comparator analyses
- `ctgov_multiarm_shared_comparator_summary_v1.csv`: trial/comparison summary
- `ctgov_multiarm_shared_comparator_notes_v1.md`: construction notes

Generated CSV, RDS, and note outputs are ignored by default. Re-run the scripts
from the project root when those artifacts are needed locally.

Cochrane was not carried forward into the shared-comparator dataset. The raw
table contains review-level comparison labels but not arm identifiers, and a
single `study.name` often appears under many comparison IDs within the same
review. That makes it hard to guarantee that retained rows are all
active-versus-the-same-comparator at the trial level, so the shared-comparator
dataset is built from clinicaltrials.gov only.
