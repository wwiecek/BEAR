
# Benchmarks of Empirical Accuracy in Research

BEAR is a “meta”-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc.

![](doc/bear_banner.png)

# Datasets included in BEAR

References, details of data availability and processing, links to raw
data, and short descriptions of each dataset are in [a separate PDF in
the main folder](datasets.pdf). Main data processing script is
[process_data.md](process_data.md) and

Here is a short summary of what’s included in BEAR:

    ## # A tibble: 20 × 7
    ##    dataset                domain              n_values n_meta n_study mean_k pct_signif
    ##    <chr>                  <chr>                  <int>  <int>   <int>  <dbl>      <dbl>
    ##  1 "Arel-Bundock et al"   political science      16649     46    2252   7.39      0.467
    ##  2 "Askarov et al"        economics              21408    352    1913  11.2       0.520
    ##  3 "Barnett and Wren"     biomedicine          1306551      1  416027   3.14      0.808
    ##  4 "Barto\u0161 et al"    exercise                2239    215    2239   1         0.263
    ##  5 "Brodeur et al"        economics               8424      1     176  47.9       0.374
    ##  6 "Chavalarias et al"    biomedicine          7935864      1 1887178   4.21      0.627
    ##  7 "Cochrane"             medicine               39768   6050   30306   1.31      0.306
    ##  8 "Costello and Fox"     ecology & evolution    88218    466   12927   6.82      0.419
    ##  9 "Head et al"           biomedicine          2010875      1  219220   9.17      0.622
    ## 10 "Jager and Leek"       biomedicine            15653      1    5322   2.94      0.777
    ## 11 "Many Labs 2"          psychology              1414     25    1414   1         0.443
    ## 12 "Metapsy"              psychotherapy           4395     20    1494   2.94      0.484
    ## 13 "Nuijten et al"        intelligence            2439      1    1913   1.27      0.529
    ## 14 "OpenSciCollab"        psychology                99      1      99   1         0.354
    ## 15 "Sladekova et al"      psychology             11540    406   11540   1         0.590
    ## 16 "What Works Clearing." education              12045      1    1408   8.55      0.334
    ## 17 "Yang et al"           ecology & evolution    17638     87    3796   4.65      0.417
    ## 18 "clinicaltrials.gov"   clinical trials        41338      1   16636   2.48      0.488
    ## 19 "EUDRA"                clinical trials         7832      1    7832   1         0.409
    ## 20 "psychology"           psychology              8514      1     721  11.8       0.358

Datasets fall into four main categories that will be useful for
different types of metascientific investigations: curated datasets of
single studies, curated sets of meta-analyses (i.e. with additional
`metaid` grouping column), large-scale scraped datasets from
PubMed/Medline, and replication datasets. Additional groupings are
available in some datasets.

    ## # A tibble: 4 × 6
    ##   gr           n_datasets n_study n_meta n_values pct_signif
    ##   <chr>             <int>   <int>  <int>    <int>      <dbl>
    ## 1 curated               6   28686     NA    80592      0.433
    ## 2 meta                  8   66467   7642   201855      0.421
    ## 3 replications          2    1513     26     1513      0.437
    ## 4 scrape                4 2527747     NA 11268943      0.648

# Modelling datasets using mixture models

## Optional post-processing

To fit mixture models described in the accompanying paper, we do minimal
postprocess (`postprocess.R`):

- Add number of observations in each study and number of studies in each
  meta-analysis
- Add study weights as 1 / (N values reported in that study)
- For large datasets (\>50,000 rows), choose one z-value per study
- Truncate very large z values (z \> 20) and replace “z = 0” statements
  with “z \< 0.5” (z=0 would not work when fitting mixtures)

In `fit_mixtures.R` we create a fit for each of the datasets, saved in
`mixtures/`. This can take a few minutes per dataset.

We calculate summaries for each dataset (e.g. probablity of
significance, replication, correct sign) in `calculate_psr.R`
