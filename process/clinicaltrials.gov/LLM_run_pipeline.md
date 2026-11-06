# Running the ClinicalTrials.gov pipeline on a new snapshot

Step-by-step instructions for an LLM coding agent. Following them, the agent
processes a new AACT snapshot, reviews the non-inferiority descriptions that
need manual classification, updates the documentation and reports back to
the maintainer. Work through the steps in order and do not skip the checks.

## 0. Before starting

1. Read, in full:
   - the root `CLAUDE.md` or `AGENTS.md`, and any parent instructions they
     name;
   - `process/clinicaltrials.gov/AGENTS.md`;
   - `process/clinicaltrials.gov/derivation_notes.md`;
   - `process/clinicaltrials.gov/non_inferiority_review_procedure.md`
     (section 2 holds the classification conventions).
2. Run every command from the BEAR project root.
3. Being given this file authorises the full pipeline run and the
   non-inferiority review below, which take longer than the five-minute limit
   in `derivation_notes.md`. Ask before any other long-running step.
4. Rules for the whole run:
   - Do not edit files in `data/` or `data_summary/` by hand. 
   - Record review decisions only through
     `review/ni_record_decisions.R`.
   - Do not weaken, skip or delete a validation check to get past a failure.
   - Do not delete anything under `data_raw/`.
   - Do not create accounts, sign in anywhere or enter credentials.
   - Keep notes of what you did, what you found and every decision you made,
     for the report in step 8.
5. Stop and ask the maintainer when:
   - the snapshot zip is missing or incomplete (step 1);
   - a script fails for a reason other than a transient one (step 2);
   - a count changes more than expected (step 3);
   - the review queue holds more than about 300 groups (step 4);
   - many queued descriptions share a pattern the rules miss (step 4);
   - a documentation change would need a judgement about content rather
     than an updated number (step 6).

## 1. Prepare

1. **Snapshot.** The maintainer downloads the AACT pipe-delimited flat-file
   zip from <https://aact.ctti-clinicaltrials.org/downloads/> and saves it as
   `data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip`, where `<DDMMYYYY>` is
   the snapshot date. If it is not there, stop and ask. Do not unzip it.
2. **Tables.** Check that the zip holds every table the pipeline reads:

   ```sh
   unzip -l data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip
   ```

   Required: `baseline_counts.txt`, `baseline_measurements.txt`,
   `browse_conditions.txt`, `browse_interventions.txt`, `conditions.txt`,
   `design_groups.txt`, `designs.txt`, `interventions.txt`, `keywords.txt`,
   `outcome_analyses.txt`, `outcome_analysis_groups.txt`,
   `outcome_counts.txt`, `outcome_measurements.txt`, `outcomes.txt`,
   `result_groups.txt`, `sponsors.txt` and `studies.txt`. If any is missing,
   stop and ask.
3. **Git.** Check `git status`. Work on a branch from `main`, e.g.
   `ctgov-snapshot-<DDMMYYYY>`. If the working tree has uncommitted changes
   other than the `data` submodule, ask before continuing.
4. **Baseline.** Keep the current public file for comparison, and note the
   snapshot it came from:

   ```sh
   cp data/clinicaltrialsgov.rds data_raw/clinicaltrials.gov/derived/previous_clinicaltrialsgov.rds
   cat data_raw/clinicaltrials.gov/derived/snapshot_stamp.txt
   ```

## 2. Run the pipeline

1. Run the pipeline in the background and write a log:

   ```sh
   CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip \
     Rscript --vanilla process/clinicaltrials.gov/run_pipeline.R \
     > data_raw/clinicaltrials.gov/run_pipeline_<DDMMYYYY>.log 2>&1
   ```

   With a new snapshot, `lib/snapshot.R` removes the cached extracts and
   rebuilds them, so the run reads the large AACT tables again. 
2. The run succeeded only if the log ends with `ClinicalTrials.gov pipeline
   completed.` A failed check stops the run with an error naming the script.
3. If it failed:
   - Find the failing script and check in the log. Rerun once if the cause is
     transient (memory, an interrupted process).
   - If AACT changed a table or column, or new values break a check, stop.
     Report the error and the the cause you found. 

## 3. Check the results

1. **Run log.** Read the whole log. Each check table must show `TRUE` in
   every row. The steps that record counts (00, 02, 06 and validate/03)
   report each count against the previous snapshot. Note every change.
2. **Compare with the previous public file:**

   ```r
   library(dplyr)
   new <- readRDS("data/clinicaltrialsgov.rds")
   old <- readRDS("data_raw/clinicaltrials.gov/derived/previous_clinicaltrialsgov.rds")
   summ <- function(x) tibble(
     rows = nrow(x), trials = n_distinct(x$nct_id),
     author = sum(x$effect_source == "author_reported"),
     raw = sum(x$effect_source == "raw_derived"),
     in_bear = sum(x$include_in_bear), columns = ncol(x)
   )
   bind_rows(old = summ(old), new = summ(new), .id = "file")
   setdiff(names(old), names(new)); setdiff(names(new), names(old))
   lost <- setdiff(unique(old$nct_id), unique(new$nct_id)); length(lost)
   ```

3. **What to expect.** A newer snapshot adds trials that posted results, so
   rows and trials should rise; from August 2025 to October 2026 the file
   grew from 151,248 to 164,453 rows. A few trials can drop out when they
   are updated. Stop and report before step 4 if:
   - rows, trials, author-reported or raw-derived rows fall;
   - the column set changes;
   - more than 1% of the previous trials are missing;
   - any count grows much faster than the time between snapshots suggests
     (for example more than 10% for a gap of a few months);
   - the author-reported share moves by more than 2 percentage points.
4. **Data summary.** `data_summary/clinicaltrialsgov.md` is rewritten by
   steps 09 and 10. Read `git diff data_summary/clinicaltrialsgov.md` and
   note any distribution that changes sharply.

## 4. Review the non-inferiority queue

1. Read step 10's message in the log. It reports how many descriptions have
   a recorded decision and, if any need review, how many descriptions and
   rows are in the queue. If nothing needs review, go to step 5.
2. Follow sections 1.7 and 2 of `non_inferiority_review_procedure.md`
   exactly.
3. Apply the decisions and rerun the final validation:

   ```sh
   CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip \
     Rscript --vanilla process/clinicaltrials.gov/process/10_integrate_manual_review.R
   CTGOV_SNAPSHOT_ZIP=data_raw/clinicaltrials.gov/<DDMMYYYY>/<file>.zip \
     Rscript --vanilla process/clinicaltrials.gov/validate/05_validate_final_merge.R
   ```
   Confirm that step 10 no longer reports descriptions needing review and
   that every check passes. Repeat steps 4.2 and 4.3 until the queue is
   empty.
4. Update the counts in section 1.9 of the procedure ("As of ... the file
   holds ..."): the date, the snapshot, the number of decisions and rows
   covered, and the counts by code:

   ```r
   library(dplyr); library(purrr)
   source("process/clinicaltrials.gov/lib/non_inferiority.R")
   dec <- read.csv("process/clinicaltrials.gov/lib/manual_review/non_inferiority_review.csv")
   d <- readRDS("data/clinicaltrialsgov.rds") %>% filter(non_inferior_desc_exists %in% TRUE)
   nrow(dec); table(dec$reviewed_type)
   sum(ctgov_ni_description_id(d$non_inferiority_description) %in% dec$description_id)
   ```

## 5. Regenerate generated documentation

1. Rebuild the dataset dictionary summaries:

   ```sh
   Rscript --vanilla doc/datasets/build_clinicaltrials_gov_dictionary.R
   ```

   This rewrites `doc/datasets/clinicaltrials_gov_dictionary.md` from the
   dictionary CSV and the new file. Do not edit the markdown by hand.

## 6. Update documentation by hand

Compute the numbers once:

```r
library(dplyr)
d <- readRDS("data/clinicaltrialsgov.rds")
bear <- d %>% filter(include_in_bear, study_type == "INTERVENTIONAL",
                     allocation == "RANDOMIZED", n_effect_rows_per_study < 20)
c(rows = nrow(d), studies = n_distinct(d$nct_id),
  pct_author = round(100 * mean(d$effect_source == "author_reported"), 1),
  bear_rows = nrow(bear), bear_studies = n_distinct(bear$nct_id))
per <- d %>% group_by(nct_id) %>% summarise(o = n_distinct(outcome_id), r = n())
median(per$o); median(per$r)
round(100 * mean(d$non_inferiority_type %in% c(
  "NON_INFERIORITY", "EQUIVALENCE", "NON_INFERIORITY_OR_EQUIVALENCE",
  "NON_INFERIORITY_OR_EQUIVALENCE_LEGACY")), 1)
desc <- d %>% filter(non_inferior_desc_exists %in% TRUE)
nrow(desc)
round(100 * prop.table(table(desc$non_inferior_type_constructed)), 1)
sum(!is.na(d$non_inferior_margin))
```

Then update these places. Find them by their text; line numbers shift.

| File | Text to update |
|---|---|
| `doc/datasets/clinicaltrials_gov.Rmd` | **Snapshot date** |
| | "It contains ... rows from ... studies; ...% are author-reported and ...% are derived from raw data" (`rows`, `studies`, `pct_author` and 100 minus it) |
| | "subset used in the main BEAR release contains ... rows from ... studies" (`bear_rows`, `bear_studies`) |
| | "The median study contributes one outcome ID and two rows" (the two medians, in words) |
| | "(...% of all effect rows)" in the `non_inferiority_type` paragraph |
| | "Of the ... author-reported rows with a description, ...% are classified as superiority ..." (`nrow(desc)`, the five shares) and "... rows have an extracted margin" |
| `process/clinicaltrials.gov/derivation_notes.md` | "Source Snapshot": the current snapshot date and zip path; add the previous snapshot to "earlier releases" |
| `doc/references.bib` | `year` of `clinicaltrials2025`, if the snapshot is in a new year (keep the key) |
| `process/clinicaltrials.gov/non_inferiority_review_procedure.md` | Section 1.9 counts (step 4.4), and any new convention in section 2 |

Do not change the measure-family shares under "Study characteristics" in
the dataset page ("About half of all effects ..."); list them in the report
for the maintainer to check.

Add new entries:

- `process/clinicaltrials.gov/changelog.md`: a dated entry with the
  snapshot, rows and trials against the previous file, and the number of new
  review decisions and conventions. Follow the style of earlier entries.
- `NEWS.md`: one bullet under the current "in development" heading, e.g.
  "ClinicalTrials.gov updated to the <date> AACT snapshot (<rows> rows from
  <studies> studies)", unless an existing bullet already covers it. Keep it
  to about 80 characters.
- Any maintainer log that the local instructions require.

Style for `doc/datasets/*.Rmd`: publication prose, no R code, no "above" or
"below"; refer to named sections.

No other documentation should need changes for a new snapshot. 

## 7. Final checks

1. `git status` should show only:
   - the `data` submodule (new `clinicaltrialsgov.rds`, not committed here);
   - `data_summary/clinicaltrialsgov.md`;
   - `doc/datasets/clinicaltrials_gov_dictionary.md`;
   - `process/clinicaltrials.gov/lib/manual_review/non_inferiority_review.csv`
     (if decisions were recorded);
   - the hand-edited files of step 6.

   Explain any other change or undo it.
2. Nothing under `data_raw/` or `pdf/` is tracked.
3. `BEAR.rds` has not been rebuilt. The maintainer verifies the row counts
   before rebuilding it.

## 8. Report to the maintainer

Report, concisely:

- the snapshot (date and zip) and whether every check passed;
- a table of rows, trials, author-reported, raw-derived and `include_in_bear`
  rows, previous against new, and the number of trials lost;
- anything flagged in step 3 and what you found;
- the review: descriptions and rows queued, decisions by type, new
  conventions added, and any proposed rule change;
- the files changed, and the numbers left for the maintainer to check
  (including the measure-family shares);
- next steps for the maintainer: review and commit the changes, open the
  BEAR_data pull request with the new `clinicaltrialsgov.rds`, and rebuild
  `BEAR.rds`.
