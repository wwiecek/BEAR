
# Benchmarks of Empirical Accuracy in Research

BEAR is a “meta”-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc.

![](doc/bear_banner.png)

# Datasets included in BEAR

References, details of data processing, and short descriptions of each
dataset included are in [a separate PDF in the main
folder](datasets.pdf). Main data processing script is
[process_data.md](process_data.md) and

Here is a short summary of what’s included in BEAR:

    ## # A tibble: 20 × 7
    ##    dataset                  n_z n_meta n_study mean_k pct_signif type                     
    ##    <chr>                  <int>  <int>   <int>  <dbl>      <dbl> <chr>                    
    ##  1 Arel-Bundock et al     16649     46    2252   7.39      0.467 NA: 16649                
    ##  2 Askarov et al          21408    352    1913  11.2       0.520 mixed: 1297, observation…
    ##  3 Barnett and Wren     1306551      1  416027   3.14      0.808 NA: 1306551              
    ##  4 Bartoš et al            2239    215    2239   1         0.263 NA: 2239                 
    ##  5 Brodeur et al           8424      1     176  47.9       0.374 DID: 50, IV: 133, RCT: 8…
    ##  6 Chavalarias et al    7935864      1 1887178   4.21      0.627 NA: 7935864              
    ##  7 Cochrane               39768   6050   30306   1.31      0.306 RCT: 27616, unknown: 121…
    ##  8 Costello and Fox       88218    466   12927   6.82      0.419 NA: 88218                
    ##  9 Head et al           2010875      1  219220   9.17      0.622 NA: 2010875              
    ## 10 Jager and Leek         15653      1    5322   2.94      0.777 RCT: 4771, NA: 10882     
    ## 11 Many Labs 2             1414     25    1414   1         0.443 NA: 1414                 
    ## 12 Metapsy                 4395     20    1494   2.94      0.484 RCT: 4395                
    ## 13 Nuijten et al           2439      1    1913   1.27      0.529 NA: 2439                 
    ## 14 OpenSciCollab             99      1      99   1         0.354 RCT: 99                  
    ## 15 Sladekova et al        11540    406   11540   1         0.590 NA: 11540                
    ## 16 What Works Clearing.   12045      1    1408   8.55      0.334 quasi: 1948, RCT: 10097  
    ## 17 Yang et al             17638     87    3796   4.65      0.417 NA: 17638                
    ## 18 clinicaltrials.gov     41338      1   16636   2.48      0.488 RCT: 41338               
    ## 19 EUDRA                   7832      1    7832   1         0.409 NA: 7832                 
    ## 20 psychology              8514      1     721  11.8       0.358 NA: 8514

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
