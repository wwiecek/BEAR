# Adding New Datasets To BEAR

This is a maintainer workflow for adding durable BEAR datasets. It is based
on the SCORE addition, but the same checks apply to smaller source packages
and large scraped datasets.

## 1. Inventory The Source

Before writing processing code, record the basic shape of the source:

- source location and whether it is public, private, or manually obtained
- licence, terms of use, and any redistribution constraints
- citation key and full reference needed for `doc/references.bib`
- source universe, search dates, snapshot date, and access date
- study/record selection rule, including paper-level and result-level filters
- source extraction method, such as manual extraction, text mining, registry
  fields, package data, author-provided files, or a mix
- any source validation evidence, such as manual spot checks, parser accuracy,
  row-count checks, or source-package tests
- row unit in the source data, such as claim, estimate, study, outcome, or
  replication pair
- expected BEAR output, including whether one source produces multiple durable
  datasets
- available metadata, especially paper IDs, years, disciplines, study IDs,
  meta-analysis IDs, sample sizes, and original significance coding
- statistical inputs available for z-values, such as effect sizes, standard
  errors, confidence intervals, p-values, t, z, F, or source-specific
  converted scales

If the source mixes row units, decide which row unit each BEAR output will
represent before coding. For SCORE, the all-claims output is claim-level,
while the replication output has one row for each original claim and one row
for its matched replication.

## 2. Choose The Durable Outputs

Use one canonical `process/` script per durable dataset or output. A source
package can therefore have more than one processing script when it produces
materially different BEAR datasets. Keep scripts runnable from the project
root and use simple project-root relative paths.

Use `data_raw/` for downloaded source files, a README explaining how to obtain
or refresh those files, partial processing artifacts, validation/audit
artifacts, and occasional pre-processing when raw files are too large or
awkward to read directly. The main pipeline from raw source material to a
canonical `.rds` dataset in `data/` should be self-contained under `process/`.

Write canonical analysis outputs to `data/`. When audits are useful, write
them to a dataset-specific documentation or audit folder with clear names.
Audit files should make validation and later review easier; they are not a
replacement for a clean canonical `.rds` output.

## 3. Keep Helper Scope Narrow

Put reusable shared helpers in `R/` only when they are genuinely reused or
when they isolate tricky parsing, numerical, or validation details. Avoid
generalising source-specific logic too early. SCORE's statistical-fragment
parser is deliberately SCORE-specific because it encodes source-text quirks
and SCORE significance coding.

Prefer existing package functions to custom helpers when the package function
is clearer. For low-level numerical code, base R is fine if it is direct.

## 4. Process Linearly

Use script-first organisation:

- one to three opening lines describing the script's purpose
- `library()` calls
- `source()` calls for local helpers
- path and configuration variables used throughout the script
- a linear workflow that reads raw inputs, cleans them, validates them, and
  saves concrete artifacts

For longer scripts, use lightweight section headers such as `# Helpers -----`
or `# Validate -----`. Use comments for non-obvious domain assumptions,
compatibility repairs, parsing rules, and validation invariants. Do not
narrate obvious code line by line.

## 5. Lock The Schema

Before saving, use `transmute()` or a narrow `select()` so the output schema
is explicit. Include standard BEAR fields where they apply, such as dataset,
estimate ID, paper or study ID, year, z, z operator, effect, standard error,
sample size, weights, and useful grouping variables. Preserve source-specific
provenance fields only when they are needed for interpretation or audit.

If an effect-size scale is heterogeneous, say so in the dataset documentation.
Do not imply that raw `b` and `se` fields are directly comparable when they
come from mixed source statistics.

## 6. Validate The Output

Add validation checks that fail loudly when core assumptions are broken. Common
checks include:

- expected row counts for each canonical output
- unique estimate, claim, study, or pair IDs
- expected subsets, such as original versus replication rows
- no missing values in required metadata
- allowed operator values for z or p-value truncation fields
- known parsing examples for text-derived statistics
- missingness rates for important statistical fields
- agreement with source-coded significance or source summary counts when
  available

Save concise validation output when it will help future maintainers understand
the run. For SCORE, row counts and agreement checks are part of the durable
audit trail.

## 7. Document The Dataset

Update `doc/datasets.Rmd`, not generated TeX output. Add the citation to
`doc/references.bib` and cite it from the dataset section. The dataset section
should explain the source, row unit, z-value construction, any filtering, and
any variables needed for interpretation. Write it as paper supplementary
material rather than repository documentation.

For each dataset section, make clear whether row counts refer to the source
data or to the transformed BEAR dataset. It is fine to cite both, but label
them explicitly. State the final BEAR row unit, such as one estimate, one
claim, one replication, one site/sample estimate, or one selected statistic per
claim. When multiple source statistics are collapsed to one BEAR row, document
the selection rule at the level needed for a reader to understand the data.

Describe effect-size and z-value construction in enough detail for readers to
understand the scale of `b`, `se`, and `z`. If original-study statistics,
replication statistics, or multiple analysis subsets are represented
separately, describe how they are distinguished. Mention important grouping
variables and whether they come from source metadata or BEAR processing.

Keep `doc/datasets.Rmd` reader-facing. Raw source file names may be named when
they identify what was obtained from the source. Do not refer to internal
staging notes, processing-script paths, or repository-level artifacts such as
processed files under `data/`. If implementation details are useful for
maintainers, keep them in the relevant workflow script, audit output,
folder-level `README.md`, folder-level `changelog.md`, or this workflow note.

Update `doc/dataset_construction_reference.md` at the same time. That table is
maintainer-facing and should record how the source selected studies or records,
how source data were extracted, and the construction details that should remain
aligned with `doc/datasets.Rmd`. If no direct source citation is available, say
which local documentation or processing evidence the summary is based on.

Review the root `README.md` when a new dataset changes the public description
of the project. Update relevant README and changelog files when a folder's
purpose, workflow, or interpretation changes. For every major update, add a
concise entry to the root `changelog.md`.

## 8. Run And Preserve Evidence

Run processing scripts when feasible. If a model or full extraction would take
too long, explain why it was not rerun and run the narrower checks that are
practical. Preserve validation output so later maintainers can see the expected
row counts, selected subsets, parsing behaviour, and open assumptions.

After documentation edits, render `doc/datasets.Rmd` when feasible, or at
least run a syntax/citation check that confirms cited keys exist in
`doc/references.bib`.
