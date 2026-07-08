Parent instructions in `../AGENTS.md` apply to this repository.
Read both files before making changes in `BEAR/`.

## Maintainer Notes
- Assume scripts are run from the `BEAR` project root unless a local script says otherwise.
- For new exploratory subfolders under `explore/`, include a minimal `README.md` and a `changelog.md`.
- Keep `explore/README.md` up to date when starting new exploratory folders,
  including the folder's purpose and whether it is BEAR-internal or a
  metascience/secondary-output analysis.
- When the user wants to compare exploratory approaches, keep prior versions and add a new versioned script instead of rewriting in place.
- Put process-specific helper code under `process/`; reserve `R/` for reusable project-wide helpers.
- When adding or materially changing a dataset, consult `doc/adding_new_datasets.md`
  and keep the implementation and documentation aligned with that workflow.
- When adding or materially changing a dataset, update
  `doc/dataset_construction_reference.md` and ensure it is aligned with `doc/datasets.Rmd`.
- When adding or materially changing a dataset, update
  `doc/dataset_classification.csv`; `R/settings.R` and the website dataset
  index derive dataset classification and summary groups from that table. Keep
  variable definitions aligned with `doc/dataset_classification_dictionary.md`.
- Keep `main.R` aligned with the repository workflow when the structure of
  durable scripts or generated outputs changes.
- After regenerating any dataset artifact, verify row counts against the
  previous artifact or documented expected counts before rebuilding `BEAR.rds`
  or rendering `README.md`.
- For generated website assets, track only the canonical source artifact when
  GitHub Actions can materialize the site-local copy during render.
- When working on processing data from papers and needing context, read PDF of the paper, which will typically be located in a subfolder of `data_raw/`
- It is OK to save artefacts from reading a PDF for faster processing in the future, but keep the folders clean when you do it
- Do not track content under `data_raw/` in the repository. Use `data_raw/`
  locally for source downloads, local READMEs explaining how to obtain data,
  partial processing artifacts, validation/audit artifacts, and occasional
  pre-processing needed to make raw sources tractable. Keep essential
  acquisition details in `process/` scripts or maintainer docs that are tracked.
  Keep the main pipeline that turns raw source material into canonical `.rds`
  datasets under `process/`.
- Do not track content under `pdf/` in the repository. Use `pdf/` locally for
  papers and reference PDFs.
- Avoid committing large files in BEAR unless explicitly requested. Prefer
  ignored local storage for raw data, PDFs, generated binaries, and bulky
  intermediate artifacts.

## Current Assumptions
- Processed inputs under `data/` are available locally as `.rds` files.
- Exploratory comparisons across datasets should label when effect-size scales are not directly comparable.
- BEAR retains infinite z-values (`|z| = Inf`) when they arise from valid
  source information, such as exact zero p-values or boundary correlations.
  Drop rows for missing or invalid inputs, not solely because the derived
  z-value or Fisher's z-transformed effect is infinite.


## datasets.Rmd

In datasets.Rmd do not write down documentation for this repo.
datasets.Rmd is documentation for a future paper, not documentation of the repo.
Do not write verbatim R code in datasets.Rmd. Use publication-ready prose and mathematical notation for formulas and derivation logic.
Dataset prose under `doc/datasets/` is reused in both the stitched paper
appendix and standalone website pages, so avoid location-dependent references
such as "below" or "above"; refer to named sections instead.
Keep `doc/datasets/*.Rmd` prose precise and concise, with minimum verbiage.
