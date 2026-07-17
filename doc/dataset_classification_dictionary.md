# Dataset classification dictionary

This dictionary defines columns in `doc/dataset_classification.csv`.
Rows use the dataset keys consumed by `R/settings.R`.

| Variable | Type | Definition |
|:--|:--|:--|
| `dataset` | character | Dataset key used in BEAR settings, processed data, fitted mixtures, and website configuration. |
| `replication` | logical | Data come from a direct replication project or from matched original/replication records. |
| `meta_analysis` | logical | Source data are organized around meta-analyses, systematic reviews, or reusable meta-analytic datasets. |
| `database` | logical | Source is a standing database, registry, clearinghouse, or package database rather than a one-off paper extraction. |
| `metascience_paper` | logical | Source was assembled for a metascience paper or project studying research practice, publication bias, replication, power, or related properties. |
| `pubmed_scrape` | logical | Source relies primarily on text-mining or scraping PubMed, MEDLINE, PubMed Central, or biomedical journal records. |
| `targeted` | logical | Data collection deliberately targeted eligible studies, reviews, trials, or datasets from a defined source or topic. This is similar to the previous `curated` idea, but records the sampling/data-collection design rather than the downstream BEAR workflow class. |
| `random_sample` | logical | Data collection is best described as a random or quasi-random sample of studies, papers, trials, or records from journals, registries, PubMed/MEDLINE, or a discipline-wide frame. |
| `primary_outcome` | logical | BEAR rows are restricted to primary/focal outcomes, endpoints, or author-emphasized principal estimates, rather than a broader set of reported analyses. |
| `rct` | logical | Contains exclusively or primarily randomised controlled studies. |
| `workflow_classification` | character | Legacy BEAR workflow class used by plots and summaries: `meta`, `curated`, `scrape`, or `replications`. |
| `summary_group` | character | Website and README display group. Values should match `bear_data_summary_group_levels` in `R/settings.R`. |

`targeted` and `random_sample` describe source collection design and are not
intended to be exhaustive or mutually exclusive with variables such as
`meta_analysis`, `database`, or `metascience_paper`.
