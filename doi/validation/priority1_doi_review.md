# Priority 1 DOI review handover

In GitHub BEAR, inspect the DOI lookup functions in `R/doi_lookup.R` and
the full-reference screening in `process/reference_doi_helpers.R`.

They were used for Bartos and Metapsy. Some candidates remain unverified.
Please read this report and the attached CSV tables, then:

1. Resolve as many flagged candidates as possible. Return an adjudication
   report with source key, accepted DOI (or rejection), evidence URL and
   rationale, for an agent to incorporate explicitly in the repository.
2. Assess whether the lookup and screening can be improved without accepting
   wrong publications or versions.

## Current state (12 September 2026)

Bartos has 5 flagged references; Metapsy has 374 flagged scoped records
covering 333 distinct references. These candidates are not attached as DOIs.
High-confidence screening results and source DOIs are already attached.
Metapsy also has 284 records without a source DOI or full reference; these
need source recovery rather than adjudication of a candidate.

## Attachments and keys

- `../Bartos/derived/doi_review.csv`: all 5 flagged references and metadata.
- `../Metapsy/derived/doi_review.csv`: all 374 flagged records and metadata.
- `../Metapsy/derived/doi_multiple_references.csv`: labels with multiple references.
- `../Bartos/data/data_processed.csv`: Bartos source data.
- `../Metapsy/data/Metapsy_Jan2026.rds`: Metapsy source tables.
- `../../data/Bartos.rds` and `../../data/Metapsy.rds`: enriched outputs.
- Each dataset's `derived/doi_lookup.csv` is the full audit, and
  `derived/crossref_references_v1.rds` is the original checkpoint.

Bartos references identify source meta-analyses, not primary trials.
Use `reference` as its lookup key. For Metapsy, use the exact triple
`metaid`, `study`, `reference`; author-year labels alone are not unique.

## Flags and evidence

The CSVs contain full source references, proposed DOI, candidate title,
journal, year, author and article type, plus a `review_reason` column.
Title coverage below 0.95, journal coverage below 0.8, an unmatched first
author or year, or a non-journal-article record causes a flag. Missing
metadata also fails the relevant check. `status = matched` in the original
Crossref cache means a candidate was returned; it is not adjudication.

Metapsy's overlapping flag counts are 64 title, 249 journal, 56 author,
141 year and 21 type mismatches. Journal abbreviations and online-first
versus print years appear to account for many flags, but some candidates
are different publications. Do not relax the thresholds wholesale.

## Bartos candidates

### 10.1111/ajag.12603

Source: Sexton, B. P., & Taylor, N. F. (2019). To sit or not to sit? A systematic review and meta‐analysis of seated exercise for older adults. Australasian journal on ageing, 38(1), 15-27.

Candidate: To sit or not to sit? A systematic review and meta‐analysis of seated exercise for older adults. Australasian Journal on Ageing; 2018; first author Sexton; type journal-article.

Flag: year.

### 10.1111/jgs.15714

Source: Wu, C., Yi, Q., Zheng, X., Cui, S., Chen, B., Lu, L., & Tang, C. (2019). Effects of mind‐body exercises on cognitive function in older adults: A meta‐analysis. Journal of the American Geriatrics Society, 67(4), 749-758.

Candidate: Effects of Mind‐Body Exercises on Cognitive Function in Older Adults: A Meta‐Analysis. Journal of the American Geriatrics Society; 2018; first author Wu; type journal-article.

Flag: year.

### 10.1080/09638288.2020.1744199

Source: Khattab, Shereen, et al. "The effects of exercise on cognition post-stroke: are there sex differences? A systematic review and meta-analysis." Disability and Rehabilitation 43.25 (2021): 3574-3591.

Candidate: The effects of exercise on cognition post-stroke: are there sex differences? A systematic review and meta-analysis. Disability and Rehabilitation; 2020; first author Khattab; type journal-article.

Flag: year.

### 10.1007/s00426-019-01145-x

Source: Landrigan, Jon-Frederick, et al. "Lifting cognition: a meta-analysis of effects of resistance exercise on cognition." Psychological research 84.5 (2020): 1167-1183.

Candidate: Lifting cognition: a meta-analysis of effects of resistance exercise on cognition. Psychological Research; 2019; first author Landrigan; type journal-article.

Flag: year.

### 10.1016/j.ctim.2022.102833

Source: Liu, Dong-Mei, Li Wang, and Li-Jun Huang. "Tai Chi Improves Cognitive Function of Dementia Patients: A Systematic Review and Meta-analysis." Alternative Therapies in Health & Medicine 29.1 (2023).

Candidate: Combined Tai Chi and cognitive interventions for older adults with or without cognitive impairment: A meta-analysis and systematic review. Complementary Therapies in Medicine; 2022; first author Li; type journal-article.

Flag: title; journal; year.

## Other priority 1 outputs

Jager–Leek has 5,317 DOI mappings for 5,322 exact PMIDs. The five title
flags were checked: source titles append research-group names to otherwise
matching titles. These notes remain in its full audit. The unmatched PMIDs
are 10789664, 18427590, 10821362, 10675071 and 11289345.

SCORE exposes supplied original-paper DOIs. No replication-publication
DOIs were inferred from the original paper.

## Implementation points for review

- The generic citation lookup returns only the highest-scored candidate.
  Consider retaining several candidates and using parsed citation metadata
  to distinguish journals and versions before selection.
- Check print and online publication dates separately; the current
  citation screen only uses the returned candidate year.
- Consider a documented journal-abbreviation crosswalk and careful handling
  of encoding errors in source author names. Preserve the original text.
- Missing candidate years now stay unverified rather than crashing.
  Legacy DOI suffixes containing angle brackets are preserved.
- Keep requests sequential with delays and checkpointing. Do not rerun
  the completed searches simply to produce this report.
