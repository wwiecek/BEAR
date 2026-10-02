# BEAR v3 — currently in development

BEAR now has a website and much more comprehensive documentation of both individual
datasets and `BEAR.rds`

Additions:

- Standardised `measure` and `method` columns; added `effect_scale` (for example, raw and log).
- Added `topic` for dataset-specific subject classifications and removed the incomplete common `field` column.
- Added DOI identifiers to many datasets, differentiating between DOIs of papers
  and DOIs of meta-analyses; also `doi_replication` for replication efforts.

Some DOI assignments and `topic` classifications may be guesses; please refer to
dataset documentation.

Dataset-specific updates:

- **ClinicalTrials.gov** data is now much larger by combining author-reported results with effects derived from outcome data
- **Cochrane**
    - removed binary rows with zero or 100% event rates in both arms; these rows remain in the fuller processed dataset
    - withdrawn reviews are no longer used; other small changes to choice of data
- **WWC**: excluded subgroup results from the main dataset; fixed study IDs, now findings from the same source citation share an identifier.



# BEAR v2 — 5 June 2026 (last release)

Second BEAR release, with 26 datasets.
