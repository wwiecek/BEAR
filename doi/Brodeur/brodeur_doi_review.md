# Brodeur article DOI lookup handover

## Current status

The metadata-aware lookup was rerun on 14 September 2026 using lookup version
3. It processed 329 distinct Brodeur article titles. The complete audit is in
`derived/doi_review.csv`; the lookup output is in
`derived/brodeur_doi_lookup.csv`; and the version-3 Crossref cache is
`derived/crossref_journal_v3.rds`.

All lookup results have a selection recorded. The summary is:

| DOI selection | Review flag | Number |
|---|---:|---:|
| `metadata_confirmed` | no | 140 |
| `metadata_supported_update` | no | 2 |
| `manual_legacy_adjudication` | no | 154 |
| `manual_version_adjudication` | no | 30 |
| `manual_legacy_adjudication` | yes | 2 |
| `manual_version_adjudication` | yes | 1 |

There are therefore three audit-level review flags, but no unresolved manual
reviews: each flagged row has a `manual_decision`, `manual_doi`, and
`manual_notes`. The failed lookup row was independently verified against the
American Economic Association article record on 14 September 2026.

## Flagged rows already adjudicated

| Source title | Status | Legacy DOI | Candidate DOI | Manual decision | DOI to retain | Reason for flag |
|---|---|---|---|---|---|---|
| Crowd-out in school-based health interventions: Evidence from India's midday meals program | `no_match` | `10.1016/j.jpubeco.2021.104552` | `10.1016/j.jpubeco.2021.104552` | `keep_legacy` | `10.1016/j.jpubeco.2021.104552` | Candidate is a different JDE article; the retained DOI is the source journal article. |
| ‘Acting Wife’: Marriage Market Incentives and Labor Market Investments | `review` | `10.29303/anjani.v4i1.3091` | `10.1257/aer.20170029` | `replace_with_candidate` | `10.1257/aer.20170029` | Candidate matches the source title, journal and journal-article type; the source author is a co-author rather than the Crossref first author. |
| The Challenges of Universal Health Insurance in Developing Countries: Experimental Evidence from Indonesia's National Health Insurance | `error` | `10.1257/aer.20200523` | unavailable | `keep_legacy` | `10.1257/aer.20200523` | Crossref failed, but the AEA record confirms the exact title, journal, year and DOI: https://www.aeaweb.org/articles?id=10.1257/aer.20200523 |

The full notes, including the exact source strings and all machine checks, are
in `derived/doi_review.csv`. Any subsequent human decisions belong in
`final/doi_decisions.csv`.

## Next step

No further DOI search is required before the next processing step. An agent
continuing this work should:

1. Compare `doi_selection`, `manual_doi`, and `doi` in `derived/doi_review.csv` with
   the final mapping produced by `doi/Brodeur/adjudicate.R`.
2. Run the durable adjudication script to save the accepted mapping under
   `final/`. Keep legacy or rejected identifiers unchanged, and retain the
   audit CSV locally.
3. Regenerate the Brodeur processed artefact without making the canonical
   processor depend on the network.
4. Check row counts, pre-existing columns, and DOI changes against the current
   `data/Brodeur.rds` before rebuilding `BEAR.rds`.

Do not treat `review = TRUE` as an instruction to replace a DOI. It is an audit
flag; the recorded manual decision and bibliographic evidence determine the
durable result. If the failed Crossref request is re-run, write a new cache
version and compare it with `derived/crossref_journal_v3.rds` rather than overwriting
the existing cache.

## Review fields

- `doi` is the current selected lookup DOI before applying any manual decision.
- `manual_decision` is one of `keep_legacy`, `replace_with_candidate`,
  `replace_with_other`, or `set_missing`.
- `manual_doi` is the DOI to use after that decision.
- `doi_selection` records whether the value came from metadata confirmation,
  a supported update, or a manual legacy/version adjudication.
- `review` identifies an audit flag, not an unresolved case.

For the general workflow, see `doc/adding_new_datasets.md`, especially the
identifier-enrichment and validation sections. Do not alter the Crossref cache
or this audit merely to make the review count zero.
