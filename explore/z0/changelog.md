# Changelog

## 2026-07-08

- Clarified that this is a BEAR-internal, LLM-assisted exploratory workflow.
- Kept generated fit caches, tables, and figures out of the tracked folder;
  `fit_cochrane_z0_sensitivity.R` remains the durable source.

## 2026-07-04

- Created an exploratory workflow for Cochrane mixture sensitivity to the
  exact-zero left-censoring bound.
- The workflow fits the BEAR-subset Cochrane data at bounds 0.25, 0.5, 1,
  and 1.96, then writes fit objects, a comparison plot, and summary tables.
