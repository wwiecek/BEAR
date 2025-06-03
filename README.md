
# Benchmarks of Experimental Accuracy in Research

BEAR is a database containing z values, effect sizes and standard errors
from various scientific disciplines. It doesn’t contribute any new data,
but repackages and merges publicly available datasets in a manner which
we hope is maximally user-friendly. Our intention is to help researchers
interested in issues of replication, exchangeability, meta-analysis etc.
etc. to

![](doc/bear_banner.png)

# Sources of data in this project

Current version of the dataset includes:

    ## # A tibble: 12 × 8
    ##    dataset                      n_z n_meta median_j n_study median_k pct_signif type                                       
    ##    <chr>                      <int>  <int>    <dbl>   <int>    <dbl>      <dbl> <chr>                                      
    ##  1 Adda et al                 12273      1       NA    4940        4      0.550 RCT: 12273                                 
    ##  2 Arel-Bundock et al         16649     46     1083    2252       25      0.467 NA: 16649                                  
    ##  3 Askarov et al              21408      1       NA    1913       28      0.520 mixed: 1297, observational: 19446, RCT: 665
    ##  4 Barnett and Wren         1306551      1       NA  416027        4      0.808 NA: 1306551                                
    ##  5 Brodeur et al               8424      1       NA     176       67      0.374 DID: 50, IV: 133, RCT: 8241                
    ##  6 Cochrane                   31594   5452       10   25593        1      0.328 observational: 15878, RCT: 15716           
    ##  7 Costello and Fox           88218    232     1064   12927       18      0.419 NA: 88218                                  
    ##  8 Jager and Leek             15653      1       NA    5322        4      0.777 RCT: 4771, NA: 10882                       
    ##  9 Metapsy                     3544     16      315    1276        2      0.491 RCT: 3544                                  
    ## 10 Sladekova et al            11540    406       55    3471        1      0.590 NA: 11540                                  
    ## 11 What Works Clearinghouse    1431      1       NA     246        9      0.372 NA: 1431                                   
    ## 12 Yang et al                 17748      1       NA    3807       10      0.403 NA: 17748

References and short descriptions of datasets we included are [available
as a Google
Doc](https://docs.google.com/document/d/1ZZAEwfHS0aAELN1w1lqEO6-eeu31_WdL/edit?usp=sharing&ouid=114240127695432531696&rtpof=true&sd=true).

# How this dataset was created

## Processing of individual datasets

General methodology for creating sets of z-values:

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
- Extract or calculate `z`:
  - `z = b/se` in most datasets
  - if no SE is avaialble, but we have p-value, we do `-qnorm(p/2)` (or
    `sign(b)*qnorm(1-p/2)` to retrieve sign of z)
  - for Barnett and Wren there are no SE but we have 95% intervals; we
    switch to log scale, calculate `se` by dividing by 3.96 and `b` as
    midpoint
  - for Sladekova et al we use Fisher’s z transformation of correlation
    coefficients, `b = 0.5*log((1 + yi)/(1 - yi))`
  - for MetaPsy database it is standardised to Hedges’ $g$
- If some `z` values were truncated in some way, keep an indicator for
  that (Jager and Leek only)
- Extract year
  - if available, year of intervention (Askarov et al); if not, year of
    study;
  - in one case (Yang et al) I retrieve year of study it from study
    titles, since half of them have dates available
- Keep study indicator, labelled `studyid`
- For sets of meta-analyses, also retain `metaid` indicator
- Remove studies where z’s are NA before saving (to reduce size of data)
- Join into a single table with column `dataset`

## Optional post-processing

- Truncate very large z values
- Add number of observations in each study,
- Add number of studies in each meta-analysis
- For large datasets, choose one z-value per study
- Add study weights as 1 / (N values reported in that study)
