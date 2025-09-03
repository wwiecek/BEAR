
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

    ## # A tibble: 15 × 7
    ##    dataset                  n_z n_meta n_study mean_k pct_signif type                                       
    ##    <chr>                  <int>  <int>   <int>  <dbl>      <dbl> <chr>                                      
    ##  1 Arel-Bundock et al     16649     46    2252   7.39      0.467 NA: 16649                                  
    ##  2 Askarov et al          21408    352    1913  11.2       0.520 mixed: 1297, observational: 19446, RCT: 665
    ##  3 Barnett and Wren     1306551      1  416027   3.14      0.808 NA: 1306551                                
    ##  4 Brodeur et al           8424      1     176  47.9       0.374 DID: 50, IV: 133, RCT: 8241                
    ##  5 Chavalarias et al    7935864      1 1887178   4.21      0.627 NA: 7935864                                
    ##  6 Cochrane               31594   5452   25593   1.23      0.328 observational: 15878, RCT: 15716           
    ##  7 Costello and Fox       88218    466   12927   6.82      0.419 NA: 88218                                  
    ##  8 Head et al           2010875      1  219220   9.17      0.622 NA: 2010875                                
    ##  9 Jager and Leek         15653      1    5322   2.94      0.777 RCT: 4771, NA: 10882                       
    ## 10 Metapsy                 3544     16    1276   2.78      0.491 RCT: 3544                                  
    ## 11 <NA>                      99      1      99   1         0.354 RCT: 99                                    
    ## 12 Sladekova et al        11540    406   11540   1         0.590 NA: 11540                                  
    ## 13 What Works Clearing.    1431      1     246   5.82      0.372 NA: 1431                                   
    ## 14 Yang et al             17638     87    3796   4.65      0.417 NA: 17638                                  
    ## 15 clinicaltrials.gov     27943      1   11186   2.50      0.526 RCT: 27943

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
- Characterise `measure` of the outcome:
  - Where known, we most often have standardised mean difference, but
    also RR/probit/ratio estimates and correlations.
  - Econ and political datasets and studies of p-values do not provide
    any information on this.
- In columns `z` (z value), `b` (effect size), `se` (standard error),
  `p` (p-value). Our main aim is to extract or calculate `z`:
  - We calculate `z = b/se` in most datasets
  - if no SE is avaialble, but we have p-value, we do `-qnorm(p/2)` (or
    `sign(b)*qnorm(1-p/2)` to retrieve sign of z)
  - for Barnett and Wren we have confidence intervals for ratio
    estimates; we switch to log scale, calculate `se` by dividing by
    3.96 and define `b` as interval midpoint on the log scale
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
- `ss` denotes sample size summed across both study arms
  - Several datasets (CDSR, Sladekova, Metapsy) include sample sizes
    broken down across arms, but we do not include them here.
  - In Adda et al this is simply enrollment
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
