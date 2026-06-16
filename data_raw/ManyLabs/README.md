# Many Labs 2 source processing

This folder contains a local copy of the Many Labs 2 replication package
downloaded from OSF (<https://osf.io/fanre/>). Source-package files are kept
under `reproducibility_package/`; local audit outputs are kept under `derived/`.
The canonical BEAR processing workflow is `process/ManyLabs2.R`, which writes
`data/ManyLabs2.rds` from the source package.

The older `process_manylabs_from_source.R` script is retained as an audit
script for reproducing the former 25-analysis `psymetadata` extraction. It is
not the canonical BEAR input.

## Canonical BEAR dataset

Run from the BEAR project root:

```r
source("process/ManyLabs2.R")
```

The script uses:

- `reproducibility_package/OSFdata.zip`;
- `OSFdata/!!KeyTables/ML2_KeyTable.csv` inside the zip;
- `OSFdata/!!RawData/ML2_results_primary_all.rds` inside the zip;
- `OSFdata/!!RawData/ML2_results_secondary_all.rds` inside the zip;
- `OSFdata/!!RawData/ML2_OriginalEffects.csv` inside the zip.

Rows are built from the key table where `study.table1.include == 1`, giving
the 28 preregistered analyses included in the Many Labs 2 paper. For each
analysis, the primary result object is used when `study.primary.include == 1`;
otherwise the secondary result object is used when
`study.secondary.include == 1`.

The BEAR fields use:

- `metaid`: Many Labs 2 analysis identifier, such as `Kay.1`;
- `studyid`: the Many Labs 2 `study.source` label plus the source filename,
  so rows can be traced back to the site/sample source data;
- `source`: `"replication"`;
- `subset`: `"primary"` or `"secondary"`;
- `measure`: `"r"`, because the retained effect size is `ESCI.r`;
- `year`: `NA`, because no clean site-level year is used here.

## Replication z derivation

For each site-level replication row:

- preferred rule: `b = ESCI.r`, `se = sqrt(ESCI.var.r)`, `z = b / se`,
  `ss = stat.N`, and `z_operator = "="`;
- fallback rule: when `ESCI.var.r` is missing but `ESCI.r` and a signed
  equivalent test statistic are available, `b = ESCI.r`,
  `z = test.statistic`, `se = abs(b / z)` when `z != 0`, `ss = stat.N`, and
  `z_operator = "="`.

The fallback is needed for `Inbar.1a` and `Schwarz.1a`. `Graham.1` has
non-missing `ESCI.r` and `ESCI.var.r`.

The canonical output has 1,592 rows: the former 1,414 `psymetadata` rows plus
60 rows for `Graham.1`, 59 rows for `Inbar.1a`, and 59 rows for `Schwarz.1a`.
The 25 analyses in the old extraction match `data/psymetadata.rds` exactly on
`z`, `b`, `se`, and `ss`.

## Why this differs from the old BEAR input

Many Labs 2 reports 28 preregistered replications. The historical BEAR input
used the `manylabs2018` subset from `data/psymetadata.rds`, which contains 25
analyses. The three paper-included analyses missing from that older extraction
are:

- `Graham.1`: Moral Foundations (Graham et al., 2009);
- `Inbar.1a`: Disgust & Homophobia (Inbar et al., 2009);
- `Schwarz.1a`: Assimilation & Contrast (Schwarz et al., 1991).

All three are marked as paper/table-included primary analyses in
`ML2_KeyTable.csv` and are present in the source result objects.

## Original effects

Original-study effects are joined from `ML2_OriginalEffects.csv` by analysis
identifier and duplicated across all replication rows for that analysis. The
canonical fields are:

- `orig.b = ESCI.r`;
- `orig.se = sqrt(ESCI.var.r)` where available;
- `orig.z = orig.b / orig.se` where available, otherwise
  `testInfo.statistic`;
- `orig.ss = N`;
- `orig.p = testInfo.p.value`, falling back to numeric
  `orig.stat.p.value` from the key table;
- `orig.z_operator = "="` when `orig.z` is available.

Original-study data are unavailable for `Alter.1`, `Huang.1`, and
`Savani.3a`. Across the 28 analyses, original `b` is available for 23,
original `se` for 22, and original `ss` for 25.
