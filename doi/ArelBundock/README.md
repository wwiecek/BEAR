# Arel-Bundock DOI enrichment

Run these scripts from the BEAR project root. `article_metadata.csv` records
the 46 source publications and their reviewed classifications. The three
small CSV files in `final/` are the accepted inputs to the network-free
`process/ArelBundock.r`; keep them under version control. `doi_collection`
identifies a source publication, while `doi_study` identifies a cited primary
publication. Neither identifies an individual `question_id` meta-analysis.

To refresh the mappings:

1. Run `lookup.R` to query source publication DOIs. Review its candidate table
   against the article metadata and `review.md` before replacing
   `final/doi_mapping.csv`.
2. Run `process/ArelBundock.r` so the source data contain the collection DOIs.
3. Run `collect_study_references.R`, then `match_study_references.R` to match
   Briggs study labels to deposited references.
4. Run `lookup_missing_study_dois.R` for cited references without a DOI, then
   rerun `match_study_references.R`. Review candidate decisions against
   `study_review.md` before replacing the two study mappings in `final/`.
5. Rerun `process/ArelBundock.r` to add the accepted study DOIs.

`derived/` contains API caches, inventories and candidate tables created by
these scripts. Its contents are ignored and may be deleted after review; the
scripts recreate them on a refresh. The accepted mappings remain available
without network access. `review.md` and `study_review.md` record the matching
rules, unresolved cases and limits of the current coverage.
