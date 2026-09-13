# DOI coverage and remaining enrichment

Checked against all 23 local `data/*.rds` files on 12 September 2026.
Nine files now have a `doi` column. Coverage counts describe saved metadata,
not a claim that every DOI has been manually verified. Source-supplied,
screened lookup and review-publication identifiers are distinguished explicitly.

## Current inventory

| Dataset file | Rows with DOI / total rows | Distinct DOIs | Provenance and status |
|---|---:|---:|---|
| `ArelBundock.rds` | No column | — | No DOI column; source crosswalk needed. |
| `Askarov.rds` | No column | — | No DOI column; numeric source IDs need a crosswalk. |
| `BarnettWren.rds` | No column | — | No DOI column; 419,234 distinct source PubMed identifiers retained. |
| `Bartos.rds` | 2,166 / 2,239 | 87 | New: source and screened Crossref DOIs of meta-analyses; `doi_scope` makes this explicit. |
| `Brodeur.rds` | 16,390 / 16,390 | 329 | Existing: legacy title-ranked Crossref candidates; the legacy audit flagged 154/329 records. A metadata-aware refresh uses a separate cache. `doinumber` is a separate registration identifier. |
| `Chavalarias.rds` | No column | — | No DOI column; validate source identifier namespaces before conversion. |
| `clinicaltrialsgov.rds` | No column | — | Registry identifiers retained; no article DOI enrichment planned. |
| `Cochrane.rds` | 760,486 / 760,486 | 6,619 | Existing: review DOIs, not included primary-paper DOIs. |
| `CostelloFox.rds` | No column | — | No DOI column; primary-study bibliography needed. |
| `euctr.rds` | No column | — | Registry identifiers retained; no article DOI enrichment planned. |
| `Head.rds` | 2,010,875 / 2,010,875 | 219,867 | Existing: source article DOIs; `doi` aliases `first.doi`; PMIDs also retained. |
| `JagerLeek.rds` | 15,633 / 15,653 | 5,317 | New: Europe PMC DOI mapping; original `pubmedID` retained. |
| `Lang.rds` | 3,885 / 3,885 | 730 | Existing: source, lookup and explicitly reviewed article DOIs. |
| `ManyLabs2.rds` | No column | — | No DOI column; original-publication crosswalk needed. |
| `Metapsy.rds` | 2,871 / 4,505 | 990 | New: source and screened Crossref article DOIs; joined by database, study label and exact reference. |
| `OSC.rds` | No column | — | No DOI column; original and replication publications need separate treatment. |
| `psymetadata.rds` | No column | — | No DOI column in the combined package output, including Nuijten. |
| `SCORE_all_claims.rds` | 3,066 / 3,066 | 200 | New: source original-publication DOIs exposed separately from `studyid`. |
| `SCORE_replications.rds` | 274 / 548 | 164 | New: `doi` on 274 original rows only; `original_doi` on all 548 rows. Replication-publication DOIs remain missing. |
| `Sladekova.rds` | No column | — | No DOI column; the current output has no IDs classified as DOI-derived. |
| `Szucs.rds` | No column | — | No DOI column; source article crosswalk needed. |
| `WWC.rds` | No column | — | No DOI column; source citations available. |
| `Yang.rds` | No column | — | No DOI column; primary-study bibliography needed. |

The assembled `BEAR.rds` (491,218 rows) currently has no DOI column and was not
rebuilt for this task. This inventory concerns the source datasets in `data/`;
adding identifiers to the common schema is separate work. Existing study IDs
were retained throughout.

## Priority 1: implemented, with unresolved records retained for review

“Records” below are the stated source keys, not necessarily distinct articles.
Unassigned includes weak candidates and records lacking enough metadata.

| Output / source key | Eligible records | Source DOI records | Newly matched records | Unassigned | Weak candidates |
|---|---:|---:|---:|---:|---:|
| Bartos: distinct full references | 93 | 27 | 61 | 5 | 5 |
| Metapsy: database, study label and exact reference | 1,995 | 529 | 808 | 658 | 374 |
| Jager–Leek: distinct PMIDs | 5,322 | 0 | 5,317 | 5 | 0 unresolved |
| SCORE all claims: original papers | 200 | 200 | 0 | 0 | 0 |
| SCORE matched output: original papers | 164 | 164 | 0 | 0 | 0 |

- **Bartos:** all 66 references without source DOIs were queried. The 88 covered
  references yield 87 distinct DOIs because two references identify the same
  publication. These DOIs identify source meta-analyses, not primary trials.
  Four remaining candidates have publication-year differences; the fifth has
  a different title, journal and year. All five stay missing pending review.
- **Metapsy:** the 1,995 reference records span 1,973 database/study keys and
  1,546 unscoped study labels. Source fields include `doi`, `full_ref`,
  `full_reference` and `reference`. All 1,024 distinct missing-DOI references
  were queried: 691 passed the citation checks and 333 remain flagged. These
  correspond to 808 matched and 374 flagged scoped records. A further 284
  records have neither a source DOI nor a full reference. Eleven database/study
  keys have multiple references; joins include the exact reference to prevent
  assigning a DOI to a different publication. Legacy DOI suffixes containing
  angle brackets are preserved.
- **Jager–Leek:** exact PMID queries in Europe PMC were checked against source
  titles. Five title flags arose from research-group names appended to source
  titles; the article titles agree and the audit records this. Five PMIDs have
  no DOI in the response. All original PMIDs remain available.
- **SCORE:** no lookup was needed. The 274 replication rows retain the matched
  original publication as `original_doi`; their own `doi` remains missing.

Crossref matches were screened using title, journal, author, year and article
type. Passing these checks is not manual adjudication. Weak candidates were
excluded from the attached mapping and retained with their evidence for review.
The short handover and complete flagged-candidate tables are in
[`data_raw/doi_validation/priority1_doi_review.md`](../data_raw/doi_validation/priority1_doi_review.md).

## Reproduce and validate

Run each optional lookup stage before its ordinary processor. The lookup stages
resume saved caches; the processors do not make network requests.

| Lookup stage | Processor | Local mapping and audit directory |
|---|---|---|
| `process/Bartos_lookup_dois.R` | `process/Bartos.R` | `data_raw/Bartos/derived/` |
| `process/Metapsy_lookup_dois.R` | `process/Metapsy_process.R` | `data_raw/Metapsy/derived/` |
| `process/JagerLeek_lookup_dois.R` | `process/JagerLeek.R` | `data_raw/JagerLeek/derived/` |
| None: supplied identifiers | `process/score_all_claims.R`, `process/score_replications.R` | Source SCORE package |

The first three save `doi_mapping.rds` and `doi_lookup.csv`. Crossref checkpoints
are `crossref_references_v1.rds`; the PMID checkpoint is `pmid_to_doi_v1.rds`.
Keep these local files: without a mapping, processors retain source DOIs only
(or missing values for Jager–Leek). Use a new cache path for a deliberate refresh.
Run Crossref queries sequentially with the helper's delay to avoid rate limits.

## Metadata-aware Crossref lookups

For article records with a separately available journal, pass title, journal,
year and first author to `lookup_identifiers()` as `source_*` fields rather than
only adding them to `query`. The journal activates retrieval and ranking of
journal-article candidates using journal, title, year, author and any supplied
DOI pattern. A title or full-reference-only query is a fallback for records
without recoverable bibliographic fields, and its audit should state that
limitation.

Use `normalise_journal()` for all journal review checks. It recognises common
aliases, including leading "The" and American Economic Journal abbreviations,
that `normalise_text()` treats as mismatches. If source metadata or ranking
rules change, save the results under a new cache path and compare them with the
old lookup; do not reuse a title-ranked cache as a metadata-aware result.

Validation confirmed unchanged row counts, row order and every pre-existing
non-enrichment column in all five files, including statistical values, types,
missingness and study IDs. Both DOI test scripts passed. The processors were
also rerun to check that they reproduce identical saved objects, including the
existing SCORE validation checks. The local validation table is
`data_raw/doi_validation/priority1_validation.csv`.

## Remaining work

Priorities 2–3 and source-work rows remain follow-up work; they were not run.
Counts in this table retain the source-field inventory from 10 September 2026.

| Priority | Dataset and available metadata | Action and qualification |
|---|---|---|
| 2 | OSC: 160 distinct original-study titles in 168 rows, with authors | Search original articles from title/author metadata. Treat replication-publication identifiers separately; never relabel an original DOI as a replication DOI. |
| 2 | WWC: 1,559 citations | Extract embedded DOIs, then query full citations. Expect reports, theses and unpublished material without registered DOIs. |
| 2 | Sladekova: 3,547 current study IDs; processing already detects DOI fields | Preserve detected source DOIs as a column. For descriptive labels, recover the corresponding primary-study bibliography within each meta-analysis before searching. |
| 3 | Barnett–Wren: 419,234 distinct PubMed identifiers | Batch unique PMIDs through Europe PMC after the smaller Jager–Leek run validates coverage and throughput. Reuse mappings across datasets. |
| 3 | Chavalarias: 1,896,954 distinct article identifiers across abstract and full-text sources | Validate identifier namespaces in each source before conversion; do not assume every numeric full-text ID is a PMID. Batch validated PMIDs and retain source indicators. |
| Source work | Costello–Fox/Yang: study labels and meta-analysis membership | Recover primary-study references from each source meta-analysis; distinguish primary articles from the meta-analysis publication. |
| Source work | Arel-Bundock: 2,251 study IDs; Askarov: 2,021 numeric study IDs | Inspect source workbooks/packages for article crosswalks. Numeric IDs alone are insufficient for bibliographic search. |
| Source work | psymetadata: package datasets currently reduced to numeric/local IDs | Inspect original package fields and documentation by dataset; preserve existing identifiers before searching references. Local IDs must be scoped by source dataset. |
| Source work | ManyLabs2: 28 replication analyses, site/file study IDs | Inspect the original-effects table and key table for original-paper references. A site/file identifier does not denote a separate publication. |
| Source work | Szucs: 3,801 constructed article IDs | Locate the article metadata crosswalk for journal/article indices in the source supplement; do not search constructed IDs. |
| Separate task | Cochrane: existing `doi` identifies the review | Obtain included-study references from the review source before searching primary-paper DOIs. Preserve the review DOI and use an explicitly named primary-article field. |
| No action | ClinicalTrials.gov, EUCTR and their combined registry outputs | Retain registry identifiers; article DOI enrichment is not needed for this task. |

# Dealing with DOI lookup issues

For problematic lookups, you will write a report to hand over to another agent.
See doc/adding_new_datasets.md for more context.

You should produce a short .md report which will start with a task desctiption that
looks roughly like this:

```
In GitHub BEAR repo look up DOI lookup functionality in R/ 

It was used to look up DOIs in dataset X, but many articles were flagged for manual review. See the attachment.

Please read the .md and 

(1) Resolve as many of these conflicts as you can. The output should be a report 
    which can be handed over to an agent which will resolve issues directly in the repo
(2) Investigate the R/ functionality in BEAR to understand if DOI lookup functionality 
    could be improved to avoid these issues in future
```
