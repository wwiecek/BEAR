# BEAR v3 — in development

BEAR now has a website and much more comprehensive documentation of both individual
datasets and BEAR.rds

Additions:

- Standardised `measure` and `method` columns; added `effect_scale` (for example, raw and log).
- Added `topic` for dataset-specific subject classifications and removed the incomplete common `field` column.

Dataset-specific updates:

- ClinicalTrials.gov data is now much larger by combining author-reported results with effects derived from outcome data
- In Cochrane, removed binary rows with zero or 100% event rates in both arms. These rows remain in the fuller processed dataset.
- Excluded WWC subgroup results from the main dataset; fixed study IDs so findings from the same source citation share an identifier.



# BEAR v2 — 5 June 2026 (last release)

Second BEAR release, with 26 datasets.
