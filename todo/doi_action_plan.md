# DOI action plan

## Naming convention

Use `doi_collection` for a publication containing several meta-analyses,
`doi_meta` for an individual meta-analysis or systematic review, and
`doi_study` for an individual paper. Include only DOI levels established for
the dataset. Matched replication outputs use `doi_study` for the original
paper on both rows and `doi_replication` for the replication publication,
if known. One replication publication may cover several original papers.
Retain `doi_meta`, `doi_study` and `doi_replication` where available in
`BEAR.rds`; keep PMID, NCT ID, `studyid` and `metaid` as distinct identifiers.

## DOI coverage and remaining enrichment

The source-file inventory below was checked against all 23 local `data/*.rds`
files on 2 October 2026. Twelve files have a level-specific DOI column. These dated
counts describe saved metadata; they do not imply manual verification of every
DOI. Source-supplied, screened lookup and review-publication identifiers are
distinguished explicitly.

## Current inventory

| Dataset file | Rows with DOI / total rows | Distinct DOIs | Provenance and status |
|---|---:|---:|---|
| `ArelBundock.rds`, `doi_collection` | 16,413 / 16,649 | 45 | Source publication DOI for 45/46 meta-collection records; the book `GreGer2019` remains unresolved. This is not a primary-study identifier. |
| `ArelBundock.rds`, `doi_study` | 3,788 / 16,649 | 405 | Primary-publication DOI matched to 440 Briggs source keys across 26 source publications; no Doucouliagos rows are covered. See `doi/ArelBundock/study_review.md` for matching rules and audit. |
| `Askarov.rds` | No column | — | No DOI column; numeric source IDs need a crosswalk. |
| `BarnettWren.rds` | No column | — | No DOI column; 419,234 distinct source PubMed identifiers retained. |
| `Bartos.rds`, `doi_meta` | 2,166 / 2,239 | 87 | New: source and screened Crossref DOIs of meta-analyses; `doi_meta` makes this explicit. |
| `Brodeur.rds`, `doi_study` | 16,390 / 16,390 | 329 | Final title-level article DOI mapping, including 33 accepted version replacements, is saved locally in `doi/Brodeur/final/doi_map.csv` and validated against raw source values. `doinumber` is a separate registration identifier. |
| `Chavalarias.rds` | No column | — | No DOI column; validate source identifier namespaces before conversion. |
| `clinicaltrialsgov.rds` | No column | — | Registry identifiers retained; no article DOI enrichment planned. |
| `Cochrane.rds`, `doi_meta` | 760,486 / 760,486 | 6,619 | Existing: review DOIs, not included primary-paper DOIs. |
| `CostelloFox.rds` | No column | — | No DOI column; primary-study bibliography needed. |
| `euctr.rds` | No column | — | Registry identifiers retained; no article DOI enrichment planned. |
| `Head.rds`, `doi_study` | 2,010,875 / 2,010,875 | 219,867 | Existing: source article DOIs; `doi_study` aliases `first.doi`; 2,005,687 PMIDs retained. |
| `JagerLeek.rds`, `doi_study` | 15,633 / 15,653 | 5,317 | New: Europe PMC DOI mapping; original `pubmedID` retained. |
| `Lang.rds`, `doi_study` | 3,885 / 3,885 | 730 | Existing: source, lookup and explicitly reviewed article DOIs. |
| `ManyLabs2.rds`, `doi_replication` | 1,592 / 1,592 | 1 | Shared Many Labs 2 publication DOI; original-publication crosswalk still needed. |
| `Metapsy.rds`, `doi_study` | 3,155 / 4,505 | 1,091 | Source, screened Crossref and reviewed article DOI mappings; joined by database, study label and exact reference. Four repeated citations with conflicting source DOI suffixes were corrected against article records. |
| `OSC.rds`, `doi_replication` | 168 / 168 | 1 | Shared Open Science Collaboration publication DOI; original-publication DOIs remain unavailable. |
| `psymetadata.rds` | No column | — | No DOI column in the combined package output, including Nuijten. |
| `SCORE_all_claims.rds`, `doi_study` | 3,066 / 3,066 | 200 | New: source original-publication DOIs exposed separately from `studyid`. |
| `SCORE_replications.rds`, `doi_study` | 548 / 548 | 164 | Original paper DOI is on both matched rows. |
| `SCORE_replications.rds`, `doi_replication` | 274 / 548 | 1 | Shared SCORE publication DOI on replication rows. |
| `Sladekova.rds` | No column | — | No DOI column; the current output has no IDs classified as DOI-derived. |
| `Szucs.rds` | No column | — | No DOI column; source article crosswalk needed. |
| `WWC.rds` | No column | — | No DOI column; source citations available. |
| `Yang.rds` | No column | — | No DOI column; primary-study bibliography needed. |

The assembled `BEAR.rds` retains `doi_meta`, `doi_study` and
`doi_replication` where available.
Collection DOIs remain in their source datasets. Existing study IDs are retained.
Several original studies may share one `doi_replication`.

Within the Briggs subset, `doi_study` covers 3,788/9,810 estimate rows
(38.6%) and 440/1,374 distinct source study keys (32.0%). It is present in
26/33 Briggs source publications. The Doucouliagos subset has 0/6,839 rows
with `doi_study`; its numeric study IDs require a source-specific citation
crosswalk. These are DOI coverage rates, not estimates of how many included
studies were journal articles or had registered DOIs.

## Next actions

1. For future DOI additions, promote only the matching publication level to
   `BEAR.rds`. Bartoš and Cochrane provide `doi_meta`, while Arel-Bundock's
   `doi_collection` remains source-only. SCORE matched rows retain the original
   publication as `doi_study`; SCORE, Many Labs 2 and OSC use their project
   papers as `doi_replication`. Keep `studyid` and `metaid` unchanged. Check
   row order, statistics and all non-identifier fields after rebuilding.
2. Continue article DOI enrichment in source datasets using the priorities and
   qualifications under Remaining work. OSC original papers, WWC and Sladekova
   are the smaller near-term candidates. First validate Chavalarias identifiers;
   only then plan any large PMID conversion. Barnett–Wren is another large
   batch after lookup throughput is established.
3. Keep `studyid` and `metaid` semantics as a separate identifier review.
   Adding a DOI does not establish that source rows share a primary study.
   In particular, Bartoš source meta-analysis DOIs cannot identify primary
   trials. Retain registry and replication-site identifiers where they are
   the meaningful source units.

## Priority 1: implemented, with unresolved records retained for review

“Records” below are the stated source keys, not necessarily distinct articles.
Unassigned includes weak candidates and records lacking enough metadata.

| Output / source key | Eligible records | Source DOI records | Newly matched records | Unassigned | Weak candidates |
|---|---:|---:|---:|---:|---:|
| Bartos: distinct full references | 93 | 27 | 61 | 5 | 5 |
| Metapsy: database, study label and exact reference | 1,995 | 529 | 949 | 513 | 183 |
| Jager–Leek: distinct PMIDs | 5,322 | 0 | 5,317 | 5 | 0 unresolved |
| SCORE all claims: original papers | 200 | 200 | 0 | 0 | 0 |
| SCORE matched output: original papers | 164 | 164 | 0 | 0 | 0 |

- **Bartos:** all 66 references without source DOIs were queried. The 88 covered
  references yield 87 distinct DOIs because two references identify the same
  publication. These DOIs identify source meta-analyses, not primary trials.
  Four remaining candidates have publication-year differences; the fifth has
  a different title, journal and year. All five stay missing pending review.
- **Metapsy:** the 1,995 reference records span 1,973 database/study keys and
  1,546 unscoped study labels. The dataset authors supplied a DOI for 529
  reference records. Metadata-aware Crossref searches supplied 949 further
  assignments; reviewed mappings add four previously unassigned records. Thus
  1,482 reference records have an attached DOI. Of the remaining 513, 173 have
  a Crossref candidate retained for review and 330 have no accepted candidate.
  Source fields include `doi`, `full_ref`,
  `full_reference` and `reference`. Eleven database/study keys have multiple
  references; joins include the exact reference to prevent assigning a DOI to
  a different publication. Legacy DOI suffixes containing angle brackets are
  preserved.

  The current output has 3,155 DOI-bearing rows and 1,091 distinct DOIs. Eleven
  reviewed mappings are saved in `doi/Metapsy/final/manual_doi_map.csv`, including the
  known replacement cases; known false-positive candidates remain missing. The
  metadata-aware refresh uses `doi/Metapsy/derived/crossref_references_v3.rds` and can
  be resumed without replacing the established mapping.
- **Jager–Leek:** exact PMID queries in Europe PMC were checked against source
  titles. Five title flags arose from research-group names appended to source
  titles; the article titles agree and the audit records this. Five PMIDs have
  no DOI in the response. All original PMIDs remain available.
- **SCORE:** no lookup was needed. The 274 replication rows retain the matched
  original publication as `doi_study` on both matched rows;
  `doi_replication` identifies the shared SCORE publication on replication rows.

Crossref matches were screened using title, journal, author, year and article
type. Passing these checks is not manual adjudication. Weak candidates were
excluded from the attached mapping and retained with their evidence for review.
The short handover and complete flagged-candidate tables are in
[`doi/validation/priority1_doi_review.md`](../doi/validation/priority1_doi_review.md).

## Reproduce and validate

Run each optional lookup stage before its ordinary processor. The lookup stages
resume saved caches; the processors do not make network requests.

| Lookup stage | Processor | Local mapping and audit directory |
|---|---|---|
| `doi/Bartos/lookup.R` | `process/Bartos.R` | `doi/Bartos/` |
| `doi/Metapsy/lookup.R` | `process/Metapsy_process.R` | `doi/Metapsy/` |
| `doi/JagerLeek/lookup.R` | `process/JagerLeek.R` | `doi/JagerLeek/` |
| None: supplied identifiers | `process/score_all_claims.R`, `process/score_replications.R` | Source SCORE package |

The first three save `final/doi_mapping.rds` and `derived/doi_lookup.csv`.
Crossref checkpoints are in `derived/`; the PMID checkpoint is likewise in
`derived/`. These local files are ignored: without a mapping, processors retain
source DOIs only (or missing values for Jager–Leek). Use a new cache path for a
deliberate refresh.
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

On 14 September 2026, the Bartoš, Metapsy, Jager–Leek, Lang, Brodeur and Head
processors were rerun using the local files under `doi/`. All six reproduced
their prior rows, schemas and non-identifier values. DOI assignments were
unchanged; trimming Head DOI whitespace recovered 208 existing PMID matches,
for 2,005,687 populated PMIDs. The shared DOI lookup tests passed. The local
validation table is
`doi/validation/derived/priority1_validation.csv`.

## Remaining work

Priorities 2–3 and source-work rows remain follow-up work; they were not run
as part of the 14 September inventory. Counts in this table retain the
source-field inventory from 10 September 2026.

| Priority | Dataset and available metadata | Action and qualification |
|---|---|---|
| 2 | OSC: 160 distinct original-study titles in 168 rows, with authors | Search original articles from title/author metadata. Treat replication-publication identifiers separately; never relabel an original DOI as a replication DOI. |
| 2 | WWC: 1,559 citations | Extract embedded DOIs, then query full citations. Expect reports, theses and unpublished material without registered DOIs. |
| 2 | Sladekova: 3,547 current study IDs; processing already detects DOI fields | Preserve detected source DOIs as a column. For descriptive labels, recover the corresponding primary-study bibliography within each meta-analysis before searching. |
| 3 | Barnett–Wren: 419,234 distinct PubMed identifiers | Batch unique PMIDs through Europe PMC after the smaller Jager–Leek run validates coverage and throughput. Reuse mappings across datasets. |
| 3 | Chavalarias: 1,896,954 distinct article identifiers across abstract and full-text sources | Validate identifier namespaces in each source before conversion; do not assume every numeric full-text ID is a PMID. Batch validated PMIDs and retain source indicators. |
| Source work | Costello–Fox/Yang: study labels and meta-analysis membership | Recover primary-study references from each source meta-analysis; distinguish primary articles from the meta-analysis publication. |
| Source work | Arel-Bundock: 934 unmatched Briggs keys; 6,839 Doucouliagos rows | Extract included-study bibliographies and keys from source articles, supplements and replication files, starting with Briggs groups with no matches and the `MunRam2021` appendix. Resolve 36 ambiguous Briggs keys against full citations; review 11 plausible non-journal items separately. Build source-specific citation crosswalks before searching Doucouliagos numeric IDs. Keep `doi_collection` separate from `doi_study`; retain missing values where no primary-paper DOI is established. |
| Source work | Askarov: 2,021 numeric study IDs | Inspect source workbooks/packages for article crosswalks. Numeric IDs alone are insufficient for bibliographic search. |
| Source work | psymetadata: package datasets currently reduced to numeric/local IDs | Inspect original package fields and documentation by dataset; preserve existing identifiers before searching references. Local IDs must be scoped by source dataset. |
| Source work | ManyLabs2: 28 replication analyses, site/file study IDs | Inspect the original-effects table and key table for original-paper references. A site/file identifier does not denote a separate publication. |
| Source work | Szucs: 3,801 constructed article IDs | Locate the article metadata crosswalk for journal/article indices in the source supplement; do not search constructed IDs. |
| Separate task | Cochrane: `doi_meta` identifies the review | Obtain included-study references from the review source before searching primary-paper DOIs. Preserve the review DOI and use an explicitly named primary-article field. |
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
