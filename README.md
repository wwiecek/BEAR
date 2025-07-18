
# Benchmarks of Experimental Accuracy in Research

BEAR is a “meta”-database containing z values, effect sizes and standard
errors from existing databases in various scientific disciplines. It
doesn’t contribute any new data, but repackages and merges what’s
publicly available in a manner which we hope is maximally user-friendly.
Our intention is to help researchers interested in issues of
replication, exchangeability, meta-analysis etc. etc.

![](doc/bear_banner.png)

# Contents of the dataset and this repo

Current version of the dataset includes:

    ## # A tibble: 14 × 7
    ##    dataset                      n_z n_meta n_study mean_k pct_signif type                                       
    ##    <chr>                      <int>  <int>   <int>  <dbl>      <dbl> <chr>                                      
    ##  1 Adda et al                 12273      1    4940   2.48      0.550 RCT: 12273                                 
    ##  2 Arel-Bundock et al         16649     46    2252   7.39      0.467 NA: 16649                                  
    ##  3 Askarov et al              21408    352    1913  11.2       0.520 mixed: 1297, observational: 19446, RCT: 665
    ##  4 Barnett and Wren         1306551      1  416027   3.14      0.808 NA: 1306551                                
    ##  5 Brodeur et al               8424      1     176  47.9       0.374 DID: 50, IV: 133, RCT: 8241                
    ##  6 Chavalarias et al        7935864      1 1887178   4.21      0.627 NA: 7935864                                
    ##  7 Cochrane                   31594   5452   25593   1.23      0.328 observational: 15878, RCT: 15716           
    ##  8 Costello and Fox           88218    466   12927   6.82      0.419 NA: 88218                                  
    ##  9 Head et al               2010875      1  219867   9.15      0.622 NA: 2010875                                
    ## 10 Jager and Leek             15653      1    5322   2.94      0.777 RCT: 4771, NA: 10882                       
    ## 11 Metapsy                     3544     16    1276   2.78      0.491 RCT: 3544                                  
    ## 12 Sladekova et al            11540    406    3471   3.32      0.590 NA: 11540                                  
    ## 13 What Works Clearinghouse    1431      1     246   5.82      0.372 NA: 1431                                   
    ## 14 Yang et al                 17748      1    3807   4.66      0.403 NA: 17748

References and short descriptions of datasets we included are [available
as a Google
Doc](https://docs.google.com/document/d/1ZZAEwfHS0aAELN1w1lqEO6-eeu31_WdL/edit?usp=sharing&ouid=114240127695432531696&rtpof=true&sd=true).
Summaries and details about contents of each dataset (e.g. how study IDs
are coded) are in the [summary
spreadsheet](https://docs.google.com/spreadsheets/d/1x2A4pgNDfrXRTdzI_LX219twXDXPPPiS7DRw1s6dHyo/edit?gid=0#gid=0)

The dataset is created in `process_data.R`. Brief data dictionary is as
follows:

- `studyid` is study indicator
  - each dataset has different naming convention (which we retain from
    original authors) but for many we have access to DOIs or PMIDs and
    they can be cross-referenced
- For sets of meta-analyses, also retain `metaid` indicator
- Characterise the method used to obtain the estimates:
  - `RCT` is the main category we care about (available in Adda et al,
    Cochrane database, Askarov, Brodeur)
  - in Jager and Leek there is no classification done by the authors,
    but we can look for keywords “randomised”, “randomized” and
    “controlled” in study titles to categorise them as RCTs
  - `mixed` category in Askarov et al means “experimental and
    observational”
  - `observational` means non-RCT in Cochrane
  - we have a few instrumental variable and differences-in-differences
    estimates from Brodeur et al
  - all other datasets mix various types of effects and are therefore
    `NA`
- In columns `z` (z value), `b` (effect size), `se` (standard error),
  `p` (p-value). Our main aim is to extract or calculate `z`:
  - We calculate `z = b/se` in most datasets
  - if no SE is avaialble, but we have p-value, we do `-qnorm(p/2)` (or
    `sign(b)*qnorm(1-p/2)` to retrieve sign of z)
  - for Barnett and Wren there are no SE but we have 95% intervals; we
    switch to log scale, calculate `se` by dividing by 3.96 and `b` as
    midpoint
  - for Sladekova et al we use Fisher’s z transformation of correlation
    coefficients, `b = 0.5*log((1 + yi)/(1 - yi))`
  - for MetaPsy database it is standardised to Hedges’ $g$
- `z_operator` column denotes if `z` values were truncated in some way
  (usually because they were extracted as statements about p values such
  as “p \< 0.05”, “p \< 0.001”, “p \> 0.5”), we retain that information
- `year` denotes, if available, year of intervention (Askarov et al); if
  not, year of study;
  - in one case (Yang et al) I retrieve year of study from study titles,
    since half of them have dates available
- Remove studies where z’s are NA before saving (to reduce size of data)

## Optional post-processing

We fit mixtures of half-normals to each dataset in `fit_mixtures.R`. To
do that, we do some additional post-processing

- Add number of observations in each study and number of studies in each
  meta-analysis
- Add study weights as 1 / (N values reported in that study)
- For large datasets, choose one z-value per study
- Truncate very large z values (z \> 20) and replace “z = 0” statements
  with “z \< 0.5”
