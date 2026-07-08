# Explore Folder

This folder contains exploratory analyses that are useful while developing
BEAR but are not part of the main rebuild path. Scripts here may depend on
current local artifacts under `data/`, `data_raw/`, `mixtures/`, or `results/`.

Exploration folders are usually LLM-developed maintainer workflows. Treat them
as scratch analysis code, not polished paper documentation.

New exploratory folders should usually contain:

- a versioned script such as `analysis_name_v1.R`
- a short `README.md` explaining the question, inputs, run command, and outputs
- a `changelog.md` when the folder's purpose or interpretation changes
- generated outputs under `output/`, `figures/`, or clearly named local files

Do not make downstream workflows depend on `explore/` without moving the
relevant code into `process/`, `workflow/`, `R/`, `paper/`, or `site/`.

BEAR-internal explorations are tracked by default when they are clean and lean.
External metascience or secondary-output explorations are ignored by default.
