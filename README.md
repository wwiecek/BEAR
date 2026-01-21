
# Benchmarks of Empirical Accuracy in Research

BEAR is a “meta”-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc. The current
release includes millions of z-values, including about 100,000 studies
from curated datasets across a diverse range of fields.

**If you only want to grab data, head to the GitHub Releases page and
grab `BEAR.rds` [at this
link](https://github.com/wwiecek/BEAR/releases).** If you want to do
more data work yourself, see *Downloading BEAR data* below. To re-run
everyting, see `main.R`.

![](doc/bear_banner.png)

# Datasets included in BEAR

References, details of data availability and processing, links to raw
data, and short descriptions of each dataset are in [a separate PDF in
the doc folder](doc/datasets.pdf).

Here is a short summary of what’s included in BEAR:

    ## # A tibble: 20 × 7
    ##    dataset              domain              n_values n_meta n_study mean_k pct_signif
    ##    <chr>                <chr>                  <int>  <int>   <int>  <dbl>      <dbl>
    ##  1 Arel-Bundock et al   political science      16649     46    2252   7.39      0.467
    ##  2 Askarov et al        economics              21408    352    1913  11.2       0.520
    ##  3 Barnett and Wren     biomedicine          1306551      1  416027   3.14      0.808
    ##  4 Bartos et al         exercise                2239    215    2239   1         0.263
    ##  5 Brodeur et al        economics               8424      1     176  47.9       0.374
    ##  6 Chavalarias et al    biomedicine          7935864      1 1887178   4.21      0.627
    ##  7 Cochrane             medicine & health      39768   6050   30306   1.31      0.306
    ##  8 Costello and Fox     ecology & evolution    88218    466   12927   6.82      0.419
    ##  9 Head et al           biomedicine          2010875      1  219220   9.17      0.622
    ## 10 Jager and Leek       biomedicine            15653      1    5322   2.94      0.777
    ## 11 Many Labs 2          psychology              1414     25    1414   1         0.443
    ## 12 Metapsy              psychotherapy           4395     20    1494   2.94      0.484
    ## 13 Nuijten et al        intelligence            2439      1    1913   1.27      0.529
    ## 14 OpenSciCollab        psychology                99      1      99   1         0.354
    ## 15 Sladekova et al      psychology             11540    406   11540   1         0.590
    ## 16 What Works Clearing. education              12045      1    1408   8.55      0.334
    ## 17 Yang et al           ecology & evolution    17638     87    3796   4.65      0.417
    ## 18 clinicaltrials.gov   clinical trials        41338      1   16636   2.48      0.488
    ## 19 EUDRA                clinical trials         8651      1    8651   1         0.408
    ## 20 psymetadata          psychology              8514      1     721  11.8       0.358

Datasets fall into four main categories that will be useful for
different types of metascientific investigations: curated datasets of
single studies, curated sets of meta-analyses (i.e. with additional
`metaid` grouping column), large-scale scraped datasets from
PubMed/Medline, and replication datasets. Additional groupings are
available in some datasets.

    ## # A tibble: 4 × 6
    ##   gr           n_datasets n_study n_meta n_values pct_signif
    ##   <chr>             <int>   <int>  <int>    <int>      <dbl>
    ## 1 curated               6   29505     NA    81411      0.432
    ## 2 meta                  8   66467   7642   201855      0.421
    ## 3 replications          2    1513     26     1513      0.437
    ## 4 scrape                4 2527747     NA 11268943      0.648

# Downloading BEAR data

If you only want to grab data, head to the GitHub Releases page and grab
`BEAR.rds` [at this link](https://github.com/wwiecek/BEAR/releases).
Alternatively, in command line do

    curl -L -o BEAR.rds https://github.com/wwiecek/BEAR/releases/download/v1/BEAR.rds

You can also re-generate that file yourself. To work with individual
datasets or to do more manipulation of inputs, they are included as a
“submodule” repo in `data/`. After cloning this repository, you need the
following command

    git submodule update --init --recursive

In other words, downloading all of the input data is opt-in rather than
part of this repo, to keep the repo size minimal.

# Modelling datasets using mixture models

Once `BEAR.rds` exists, you may choose to fit our preferred mixture
models to the sets of z-values. To fit models described in the
accompanying paper, we first do minimal postprocess
(`workflow/postprocess.R`):

- Add number of observations in each study and number of studies in each
  meta-analysis
- Add study weights as 1 / (N values reported in that study)
- For large datasets (\>50,000 rows), choose one z-value per study
- Truncate very large z values (z \> 20) and replace “z = 0” statements
  with “z \< 0.5” (z=0 would not work when fitting mixtures)

In `workflow/fit_mixtures.R` we create a fit for each of the datasets,
saved in `mixtures/`. This can take a few minutes per dataset. We
include the up-to-date results in the repo.

We calculate summaries for each dataset (e.g. probablity of
significance, replication, correct sign) in `workflow/calculate_psr.R`

# Results of mixture modelling

    ## # A tibble: 15 × 6
    ##    dataset              omega   PoS PoS_80 replication  sign
    ##    <chr>                <dbl> <dbl>  <dbl>       <dbl> <dbl>
    ##  1 Askarov et al        0.714 0.482  0.293       0.467 0.879
    ##  2 Nuijten et al        0.848 0.481  0.289       0.467 0.88 
    ##  3 ctgov / EU CTR       0.946 0.468  0.282       0.453 0.873
    ##  4 Metapsy              0.831 0.463  0.264       0.447 0.875
    ##  5 Arel-Bundock et al   0.652 0.401  0.202       0.384 0.853
    ##  6 Costello and Fox     0.794 0.387  0.235       0.37  0.791
    ##  7 Brodeur et al        0.786 0.358  0.17        0.339 0.833
    ##  8 psymetadata          0.666 0.329  0.211       0.31  0.722
    ##  9 What Works Clearing. 0.879 0.319  0.17        0.301 0.756
    ## 10 Cochrane             0.705 0.231  0.096       0.21  0.689
    ## 11 Bartos et al         0.822 0.226  0.083       0.204 0.756
    ## 12 Many Labs 2          0.992 0.435  0.346       0.421 0.755
    ## 13 OpenSciCollab        1     0.355  0.222       0.337 0.784
    ## 14 Chavalarias et al    0.197 0.512  0.32        0.499 0.891
    ## 15 Barnett and Wren     0.083 0.319  0.127       0.299 0.821

`omega` is relative publication probability based on crossing of the
\|z\|=1.96 threshold; `PoS` is probability of significance (assurance)
and `PoS_80` is proportion of studies that reach 80% power;
`replication` and `sign` probabilities are described in accompanying
paper.
