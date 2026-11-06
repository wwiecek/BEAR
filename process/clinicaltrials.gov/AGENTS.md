Instructions in the repository's root `AGENTS.md` also apply here.

## Running the pipeline on a new snapshot

Follow `LLM_run_pipeline.md` step by step when asked to process a new AACT
snapshot: preparation, the run, checks against the previous file, the
non-inferiority review, documentation updates and the report.

## Non-inferiority test type and margin

The columns `non_inferior_desc_exists`, `non_inferior_type_constructed` and
`non_inferior_margin` of `data/clinicaltrialsgov.rds` come from rules in
`lib/non_inferiority.R` (step 07) and from recorded manual review decisions
in `lib/manual_review/non_inferiority_review.csv` (step 10).

- Read `non_inferiority_review_procedure.md` and follow it before reviewing
  non-inferiority descriptions, editing the decisions file, or changing
  `lib/non_inferiority.R`. Section 2 lists the classification conventions;
  apply them as written and add any new convention to that section.
- After every pipeline run, check step 10's message. If it reports
  descriptions that need manual review, tell the user how many descriptions
  and rows, and offer to review them following the procedure. Until they are
  reviewed, those rows keep rule-based values.
- Use the helper scripts in `review/`: `ni_print_queue.R` (queue grouped into
  near-duplicates), `ni_record_decisions.R` (record decisions by id or group)
  and `ni_compare_rules.R` (list every changed extraction after a rule
  change).
- Record decisions only through the decisions file, with a note for anything
  not obvious; never edit the output data directly. Rerun step 10 afterwards
  and confirm that the queue is empty and its checks pass.
- After a review or a rule change, add a dated entry to `changelog.md` with
  the number of decisions and any new convention.
