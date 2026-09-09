# BEAR v3 — in development

- Standardised `measure` and `method` across datasets and added `effect_scale`, retaining detailed source coding in individual dataset files.
- Added `topic` for dataset-specific subject classifications, separating these from provenance and analysis subsets. Removed the incomplete common `field` column; registry topic assignments remain deferred pending review.
- Expanded the common data dictionary to cover identifiers, effect scales, z derivation and bounds on absolute z-values. Combined the website’s dictionary and derivation documentation in one Documentation page.
- Rebuilt ClinicalTrials.gov extraction to combine author-reported analyses with effects derived from registered raw outcome data, retain richer trial characteristics and substantially expand the main BEAR sample.
- Updated the Cochrane snapshot and excluded binary rows with zero events in both arms or events in every participant in both arms from main BEAR. These rows remain in the fuller processed dataset.
- Excluded WWC subgroup findings and improved study IDs so findings from the same source citation share an identifier.
- Added explicit dataset classification metadata to generate consistent website tags, groupings and documentation.
- Generalised power/sign/replication calculations to support alternative significance thresholds and sample-size multipliers.

# BEAR v2 — 5 June 2026

Second BEAR release, with 26 datasets.
