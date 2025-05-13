
# Sources of data in this project

Up-to-date document with short descriptions is [available as a Google
Doc](https://docs.google.com/document/d/1ZZAEwfHS0aAELN1w1lqEO6-eeu31_WdL/edit?usp=sharing&ouid=114240127695432531696&rtpof=true&sd=true).

General methodology for creating sets of z-values:

- Remove non-RCTs and various malformed inputs
- Extract or calculate `z`:
  - `z = b/se` in most datasets
  - if no SE is avaialble, but we have p-value, we do `-qnorm(p/2)` (or
    `sign(b)*qnorm(1-p/2)` to retrieve sign of z)
  - for Barnett and Wren there are no SE but we have 95% intervals; we
    switch to log scale, calculate `se` by dividing by 3.96 and `b` as
    midpoint
  - for Sladekova et al `b = 0.5*log((1 + yi)/(1 - yi))`
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

Post-processing:

- Truncate very large z values ourselves
- Add number of observations in each study,
- Add number of studies in each meta-analysis
- For large datasets, choose one z-value per study
- …something about calculating weights

<!-- -->

    ## # A tibble: 11 × 7
    ##    dataset              n_z n_meta median_j n_study median_k pct_signif
    ##    <chr>              <int>  <int>    <dbl>   <int>    <dbl>      <dbl>
    ##  1 Arel-Bundock       16649     46     1083    2252       25      0.467
    ##  2 Askarov            21408      1       NA    1913       28      0.520
    ##  3 BarnettWren      1305303      1       NA  415855        4      0.809
    ##  4 Brodeur             8241      1       NA     176       67      0.374
    ##  5 CDSR               40383   4057       22   23566        2      0.434
    ##  6 Costello and Fox   88218    232     1064   12927       18      0.419
    ##  7 Jager and Leek     15653      1       NA    5322        4      0.777
    ##  8 Metapsy             2532     12      315     945        4      0.520
    ##  9 Sladekova          11540    406       55   11540        1      0.590
    ## 10 WWC                 1399      1       NA     245        9      0.377
    ## 11 Yang et al         17748      1       NA    3807       10      0.403
