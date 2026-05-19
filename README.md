
# Benchmarks of Empirical Accuracy in Research

BEAR is a meta-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc. The current
release includes millions of z-values, including about 100,000 studies
from curated datasets across a diverse range of fields.

This package includes our entire workflow (in R), documentation,
results, and optional modelling of empirical research.

- **If you want to grab data only**, head to the GitHub Releases page
  and grab `BEAR.rds` [at this
  link](https://github.com/wwiecek/BEAR/releases).\*\*
- If you want to do more data work with individual datasets, see
  *Downloading BEAR data* below.
- To re-run everything, including modelling of data, see `main.R`.

![](doc/bear_banner.png)

# Datasets included in BEAR

References, details of data availability and processing, links to raw
data, and short descriptions of each dataset are in [a separate PDF in
the doc folder](doc/datasets.pdf).

Here is a short summary of what’s included in BEAR:

    ## # A tibble: 23 × 7
    ##    dataset              domain                       n_values n_meta n_study mean_k pct_signif
    ##    <chr>                <chr>                           <int>  <int>   <int>  <dbl>      <dbl>
    ##  1 Arel-Bundock et al   political science               16649     46    2252   7.39      0.467
    ##  2 Askarov et al        economics                       21408    352    1913  11.2       0.520
    ##  3 Barnett and Wren     biomedicine                     50000      1   47543   1.05      0.833
    ##  4 Bartos et al         exercise                         2239    215    2239   1         0.263
    ##  5 Brodeur et al        economics                       15917      1     328  48.5       0.357
    ##  6 Chavalarias et al    biomedicine                     50000      1   49458   1.01      0.628
    ##  7 Cochrane             medicine & health               38058   5871   29327   1.30      0.311
    ##  8 Costello and Fox     ecology & evolution             88218    466   12927   6.82      0.419
    ##  9 Head et al           biomedicine                     50000      1   48175   1.04      0.609
    ## 10 Jager and Leek       biomedicine                     15653      1    5322   2.94      0.777
    ## 11 Lang                 economics                        3885      1     736   5.28      0.576
    ## 12 Many Labs 2          psychology                       1592     28     128  12.4       0.414
    ## 13 Metapsy              psychotherapy                    4395     20    1494   2.94      0.484
    ## 14 Nuijten et al        intelligence                     2439      1    1913   1.27      0.529
    ## 15 OpenSciCollab        psychology                         97      1      97   1         0.351
    ## 16 SCORE, all claims    social & behavioural scienc…     1946      1     160  12.2       0.748
    ## 17 SCORE replications   social & behavioural scienc…      267      1     163   1.64      0.644
    ## 18 Sladekova et al      psychology                      11540    406   11540   1         0.590
    ## 19 What Works Clearing. education                       12045      1    1408   8.55      0.334
    ## 20 Yang et al           ecology & evolution             17638     87    3796   4.65      0.417
    ## 21 clinicaltrials.gov   clinical trials                 41367      1   16597   2.49      0.484
    ## 22 EUDRA                clinical trials                  8650      1    8650   1         0.414
    ## 23 psymetadata          psychology                       8514      1     721  11.8       0.358

Datasets fall into four main categories that will be useful for
different types of metascientific investigations: curated datasets of
single studies, curated sets of meta-analyses (i.e. with additional
`metaid` grouping column), large-scale scraped datasets from
PubMed/Medline, and replication datasets. Additional groupings
(e.g. clinical trial phases) are available within some datasets.
Replication datasets also include additional columns that store values
from original studies.

    ## # A tibble: 4 × 6
    ##   gr           n_datasets n_study n_meta n_values pct_signif
    ##   <chr>             <int>   <int>  <int>    <int>      <dbl>
    ## 1 curated               8   30513     NA    94763      0.436
    ## 2 meta                  8   65488   7463   200145      0.422
    ## 3 replications          3     388     30     1956      0.442
    ## 4 scrape                4  150498     NA   165653      0.698

# Downloading BEAR data

If you only want to grab data, head to the GitHub Releases page and grab
`BEAR.rds` [at this link](https://github.com/wwiecek/BEAR/releases).
Alternatively, in command line do

    curl -L -o BEAR.rds https://github.com/wwiecek/BEAR/releases/download/v1/BEAR.rds

You can also re-generate that file yourself. To work with individual
datasets or to do more manipulation of inputs, you need to take an
additional step. After cloning this repo (which only includes code and
outputs), you need to grab a “submodule” repo that has contents of
`data/` folder. In command line of this repository, do

    git submodule update --init --recursive

That will download about 100 MB of individual datasets. In other words,
downloading all of the input data files is opt-in rather than part of
this repo, to keep the repo size minimal.

If you want to re-derive each of the datasets yourself, you can see all
of data processing done for individual datasets in `process/`. For many
datasets there is nil processing done by us, but for some there are
extensive scripts to download and clean up data.

Once datasets are present in `data/`, `BEAR.rds` is stiched together in
script `workflow/build_bear.R` which does some of the processing
described in [datasets documentation](doc/datasets.pdf).

# Modelling datasets using mixture models

Once `BEAR.rds` exists, you may choose to fit our preferred mixture
models to the sets of z-values. These models are described in the
accompanying paper: [A Statistical Case for Qualified Scientific
Optimism](https://sites.stat.columbia.edu/gelman/research/unpublished/A_statistical_case_for_qualified_scientific_optimism.pdf).

Before fitting models, we first do minimal postprocess
(`workflow/postprocess.R`):

- Add number of observations in each study and number of studies in each
  meta-analysis
- Add study weights as 1 / (N values reported in that study)
- For large datasets, thin out the dataset to 50,000 rows, preferring
  one z-value per paper
- Truncate very large z values (z \> 20) and replace “z = 0” statements
  with “z \< 0.5” (z=0 would not work when fitting mixtures)

In `workflow/fit_mixtures.R` we create a fit for each of the datasets,
saved in `mixtures/`. This can take a few minutes per dataset. We
include the up-to-date results in the repo.

We calculate summaries for each dataset (e.g. probablity of
significance, replication, correct sign) in `workflow/calculate_psr.R`

# Results of mixture modelling

    ## # A tibble: 18 × 6
    ##    dataset              omega   PoS PoS_80 replication  sign
    ##    <chr>                <dbl> <dbl>  <dbl>       <dbl> <dbl>
    ##  1 Askarov et al         0.71  0.48   0.3         0.47  0.88
    ##  2 Nuijten et al         0.85  0.48   0.29        0.47  0.88
    ##  3 SCORE, all claims     0.13  0.47   0.28        0.46  0.88
    ##  4 ctgov / EU CTR        0.95  0.47   0.28        0.45  0.87
    ##  5 Metapsy               0.74  0.43   0.23        0.42  0.87
    ##  6 Lang                  0.24  0.42   0.22        0.41  0.86
    ##  7 Arel-Bundock et al    0.65  0.4    0.2         0.38  0.85
    ##  8 Costello and Fox      0.79  0.39   0.24        0.37  0.79
    ##  9 psymetadata           0.67  0.32   0.21        0.31  0.72
    ## 10 Brodeur et al         0.68  0.32   0.14        0.3   0.82
    ## 11 What Works Clearing.  0.88  0.32   0.17        0.3   0.76
    ## 12 Cochrane              0.7   0.24   0.1         0.21  0.69
    ## 13 Bartos et al          0.82  0.23   0.09        0.21  0.76
    ## 14 SCORE replications    0.69  0.5    0.33        0.49  0.87
    ## 15 Many Labs 2           0.87  0.41   0.33        0.39  0.75
    ## 16 OpenSciCollab         1     0.35   0.22        0.34  0.79
    ## 17 Chavalarias et al     0.19  0.51   0.32        0.5   0.89
    ## 18 Barnett and Wren      0.08  0.32   0.13        0.3   0.82

`omega` is relative publication probability based on crossing of the
\|z\|=1.96 threshold; `PoS` is probability of significance (assurance)
and `PoS_80` is proportion of studies that reach 80% power;
`replication` and `sign` probabilities are described in accompanying
paper.
