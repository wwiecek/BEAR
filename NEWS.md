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
- **Sladekova**: attempted to make some fixes around measured correlations;
  BEAR.rds now includes only the 17% of rows that were clearly Fisher-transformed 
  correlations; the individual file has all estimates with extra detail about scale.
- **WWC**: excluded subgroup results from the main dataset; fixed study IDs, now findings from the same source citation share an identifier.

There are a lot of small corrections to many datasets, I will only start listing
these once we have a more stable workflow for processing datasets in v4.


# BEAR v2 — 5 June 2026 (last release)

Second BEAR release, with 26 datasets.
