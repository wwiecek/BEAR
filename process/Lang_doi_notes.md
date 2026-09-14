# Lang article identifiers: corrections from the September 2026 review

The original lookup chose the highest Crossref score among three candidates.
Excellent title overlap did not distinguish journal articles from working
papers, registry entries, reproductions or book chapters. This note supersedes
`doc/lang_doi_manual_review.md` and incorporates the subsequent bibliographic
review supplied by the maintainer. The old note and pre-correction dataset are
archived locally under `doi/Lang/doi_review_20260910/`.

## Explicit corrections in Lang.R

| Source paper ID | Decision | Journal article DOI |
|---|---|---|
| 2–5 | One Abramitzky et al. article; use `studyid = Lang_paper_2`. Page endings 609–611 are source errors; the published range is 580–608. | [10.1257/aer.20191586](https://www.aeaweb.org/articles?id=10.1257/aer.20191586) |
| 29–31 | One Breza–Kinnan article; use `Lang_paper_29`. Published pages are 1447–1497. | [10.1093/qje/qjab016](https://academic.oup.com/qje/issue/136/3) |
| 42–43 | One Chen article with differently encoded citations; use `Lang_paper_42`. | [10.1257/aer.20201238](https://doi.org/10.1257/aer.20201238) |
| 472 | Droller remains a separate six-row article. | [10.1111/ecoj.12505](https://onlinelibrary.wiley.com/doi/10.1111/ecoj.12505) |
| 474 | Keep all fourteen Jessoe et al. rows together; correct the titles on source rows 2052 and 2054. | [10.1111/ecoj.12448](https://doi.org/10.1111/ecoj.12448) |
| 223 | Replace the affiliation mistakenly stored as the title with Chan–Manova, *Financial development and the choice of trade partners*. | [10.1016/j.jdeveco.2015.04.002](https://doi.org/10.1016/j.jdeveco.2015.04.002) |
| 324 | Replace the affiliation with Lavy–Zablotsky, *Women's schooling and fertility under low female labor force participation: Evidence from mobility restrictions in Israel*. | [10.1016/j.jpubeco.2015.02.009](https://doi.org/10.1016/j.jpubeco.2015.02.009) |
| 530 | Use the JDE article rather than the book/chapter version. | [10.1016/j.jdeveco.2018.07.008](https://doi.org/10.1016/j.jdeveco.2018.07.008) |
| 689 | Use the QJE article rather than the book/chapter version. | [10.1093/qje/qjx040](https://doi.org/10.1093/qje/qjx040) |
| 100 | Use the 2021 QJE article on Medicaid and mortality, not NBER working paper w26081. | [10.1093/qje/qjab004](https://doi.org/10.1093/qje/qjab004) |
| 397 | The publisher confirms Claudia Martínez A.; Crossref stores the surname as `A.`, preventing automatic author agreement. | [10.1257/app.20150245](https://www.aeaweb.org/articles?id=10.1257/app.20150245) |
| 597 | The publisher confirms César Calderón and Megumi Kubota; the source surname is truncated to `caldern`. | [10.1016/j.jinteco.2017.08.002](https://www.sciencedirect.com/science/article/pii/S0022199617300983) |

For Jessoe, the source rows already name Katrina Jessoe as first author.
Table 5 of the [author's manuscript](https://kkjessoe.faculty.ucdavis.edu/wp-content/uploads/sites/803/2024/01/WeatherLaborMexico.pdf),
PDF page 50, gives GDD 0.00000637 (SE 0.0000761) and HDD 0.000348 (SE 0.000548),
matching the disputed rows. These are incorrect titles, not misplaced estimates.
Do not move either row to Droller.

Only `studyid`, the affected `source_title` values and DOIs are corrected.
`paper_id`, `source_unique_paperid`, original citations, row order and all numeric
inputs remain intact. `source_title_original` preserves the pre-correction title.
The 736 source IDs represent 730 articles (660 in the 2,082-row main sample).
The raw Stata file is never edited. Manual DOI overrides are applied last, after
cached automatic assignments, so subsequent lookup cannot undo them.

## Journal-version audit

`Lang_lookup_dois.R` parses title, journal and first author from the 125
citation-only records for lookup without changing their source citations.
`Lang_journals.csv` supplies journal-specific DOI patterns as an additional
check, not a substitute for bibliographic matching. The shared lookup searches
up to twenty journal-article candidates and compares journal, title, publication
dates and first author before accepting a replacement. Inconsistent candidates
remain in the review table; they do not silently replace established DOIs.

The audit revisits all citation-only records, old title overlaps below 0.90,
DOI-pattern mismatches and explicit manual cases. Historical candidates remain
in `lang_doi_candidates.csv`; revised results and before/after DOIs are in
`lang_doi_journal_lookup.csv`, with unresolved metadata flags in
`lang_doi_journal_review.csv`, all under `doi/Lang/`.

The earlier review confirmed that the other fourteen low-overlap records were
correct article matches: 208, 279, 280, 570, 581, 584–586, 589–591, 627, 641
and 656. Title truncation, punctuation and source encoding explain those flags;
they are not evidence for choosing a different publication.
Their DOI assignments are also pinned in the manual override block.

The journal-specific patterns identify 38 wrong-publication/version assignments,
including paper 100, compared with 37 in the supplied report. The check also
catches paper 134: an AEA registry DOI shares the AEA publisher prefix but is
not an AEJ article.
This illustrates why publisher prefixes alone are insufficient. Jessoe's two
mislabelled rows require an additional correction independent of those 38.
The run covered 178 source IDs (172 distinct queries). Ten citation-only
records retain metadata warnings: eight date discrepancies and two author-name
encoding differences. Their candidate DOIs agree with the previous assignments;
none was replaced. These warnings remain visible in the local review table.

Run `tests/test_doi_lookup.R` for shared matching tests. The September 2026
Lang correction audit is retained locally under `doi/validation/`.
The saved `BEAR.rds` is not rebuilt here. A later rebuild will pick up the
consolidated article IDs; it will still use `Lang_paper_*`, not DOI study IDs.
