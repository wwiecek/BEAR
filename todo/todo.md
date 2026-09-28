---
editor_options: 
  markdown: 
    wrap: 72
---

# To-do items for BEAR

## Add roadmap and additions on the website

People should be able to see what's coming next This to-do should partly
be turned into description of issues for the website People should be
able to submit their own data and flag issues

## Remove replications datasets from the main release

I'd keep them on the website, but not merge into BEAR.rds We should
include pointers to FORTT and Replications Database

## Bartos studyid

In Bartos.Rmd we say

> -   **Study ID:** we use `id` from original data, which is unique to
>     each row and cannot group estimates within studies.

Could this not be fixed? I assume there will be multiple estimates per
paper. Or are we positive this is genuinely one paper, one effect?

## Continue DOI enrichment

Follow the [DOI action plan](doi_action_plan.md) for the current
inventory, priorities, identifier distinctions and remaining work.

## Arel-Bundock subsets

In Arel-Bundock et al we say

> `subset` column is the source `meta_id`

but these labels are not informative.

```         
> ArelBundock$meta_id %>% table
.
            Ahm2014          ArcNic2009       AskDouPal2021       AwaBelEst2020 
                246                  11                1645                 284 
            Bal2010  BalTybWuAntVan2018       BarTraSau2020          BelCan2017 
                 45                   5                  13                 146 
      BhaDahHan2019       BlaChrRud2020             Bro2018       BurKosLan2013 
                 58                  88                 492                  49 
      ColRosMag2020          deWBek2017        DinLuRic2021       DinSchSon2020 
               2047                 115                  79                1001 
         DouUlu2006       EfePugAdn2011         EshEtAl2021             Ger2016 
                158                 224                  21                  44 
         GreGer2019       GreMicRob2006             Hei2020             Hei2021 
                236                 315                1182                 604 
      HeiMoeYet2018    HolRanMooCro2021       HomMcCTab2015          HouCon2019 
                759                  13                  43                  34 
            Inc2020          KalBro2017       LauSigBro2007        LiOweMit2018 
                 30                  48                 306                 279 
             Lu2016              Lu2018        LuLinWan2019 MatKnoValHopSik2019 
                 60                  38                  23                  70 
         MerPhi2018          MunRam2021             OBr2019           OweLi2020 
                 19                  51                 516                 229 
            Phi2016          SchCop2021          TriWen2020         WalEtAl2020 
               1083                  67                3445                  30 
         YesYes2019         ZhaEtAl2021 
                235                 163 
```

Would we be able to code them as topics or article titles by looking up
each article?

## Review remaining `source` uses

Review the remaining uses of `source`: could they be renamed or moved to
more specific fields? Assess the other common columns at the same time.

## Documentation should also include description of the model

## Lang fixes

REview data_raw/Lang/ in detail. Is there a trace to the authors picking
one hypothesis per study systematically (focal hypothesis) or we cannot
distinguish them? When they do one-observation-per-paper does it look
like they grabbed it at random or systematically? YOu may need to read
the original paper for context. Should BEAR.rds be picking one estimate
per each MHT group? Read their documentatioin and code and give your
opinion. Would our results change if we did that?

In Lang.rds all `source_` variables, save for MHT, do not feel useful to
the consumers of BEAR. Do you agree? If yes, remove.

Some checking and validation code seems pointless here given that we
only do this data processing once, like you shouldn't check for required
columns. See if this script can be simplified while still producing
exactly the same results we had to date

Use journal as `topic`? Could use JEL as a topic variable, but maybe
there is an easier classification here?

## Small dataset-specific checks

### Issue: Sladekova study IDs and `abs(yi) > 1` brief

Current status: `process/Sladekova.R` already reconstructs study IDs
from DOI or descriptive fields where possible. In the current processed
file, 9,748 of 11,591 rows use a DOI or descriptive label; 1,843 rows
(15.9%) still fall back to row-unique IDs because no descriptive study
label was available after processing.

Short brief for Bartos: BEAR treats the Sladekova `yi` values as
correlation-scale effects and applies a Fisher z transform. In the local
source files I find 43 rows from 10 files with `abs(yi) > 1`, mainly
`B109_1.csv` (20 rows) and `A23_1.csv`/`A23_2.csv` (7 rows each). These
values cannot be ordinary correlations, so I need to know whether they
are coding errors, a different effect-size scale in those files, or rows
that should be excluded before applying the correlation-scale transform.

Potential follow-up after reply: if these rows are invalid correlations,
update `process/Sladekova.R` to drop or separately classify them before
the Fisher transform, then document the row count change in
`doc/datasets/Sladekova.Rmd`.

## Data dictionaries for richer public datasets

Short spec:

1.  Prioritise datasets with many columns or substantial metadata beyond
    `BEAR.rds`: Brodeur, Askarov, OSC, SCORE, WWC, Cochrane, Lang,
    EUCTR, and Bartos.
2.  For each dataset, inspect the saved public `.rds` and make a
    best-guess definition for every column: row unit, source, meaning,
    allowed values or units, missingness, and notes needed for
    interpretation.
3.  Check source documentation, package docs, replication files, papers,
    and online data dictionaries where available. Replace guesses with
    sourced definitions and record unresolved ambiguity.
4.  Reconsider the public `data/` schema before finalising the
    dictionary: decide which extra columns should remain in the full
    public dataset, which should be renamed or collapsed, and which are
    internal audit fields that should stay out of `data/`.
5.  Store each dictionary beside the dataset docs as
    `doc/datasets/<dataset>_dictionary.csv`, with a reproducible
    `doc/datasets/build_<dataset>_dictionary.R` script that validates
    one dictionary row per public column and writes
    `doc/datasets/<dataset>_dictionary.md` for the website.
6.  As this workflow matures, keep generalising the rules in
    `doc/adding_new_datasets.md` so future datasets follow the same
    pattern.

For the ClinicalTrials.gov dataset, run and interpret the existing
predictor analysis of how effect sizes differ according to crucial
predictors:

-   author-reported vs raw-outcome-derived
-   phase
-   domain
-   type of trial (drug, biological, device, procedure etc)
-   measure

Inspect the proportion significant and median \|z\|, including among
significant results, and decide which results matter. Then run mixture
fits for informative subgroups and compare mixtures, replication
probability and fitted summaries.

Run the existing paired comparison of author-reported and
raw-outcome-derived effect sizes, and interpret its plots and summary
tables.

## ctgov variables

aren't scale and measure_class and raw_measure redundant with each
other? wouldn't it be better to create a single categorical variable
here? how are they used in build_bear?

Most of "Raw result-group metadata" does not seem very relevant/useful
to an end user. Please review and consider which variables to retain.
Feel free to disagree.

## PubMed downloads

Can we get:

-   Number of citations
-   Article title
-   Journal
-   Journal grouping into disciplines (a la Head 2015 grouping of
    journals)
-   Classification codes (like JEL)
-   Type of experiment, prospective vs retrospective, randomised,
    controlled,

Borrow Barnett and Wren approaches

Quantify in what % of articles we have odds ratios, risk ratios,
p-values reported

Do publication bias assessment only on articles that have 1 p-value in
the abstract then compare it to an assessment where we have multiple
p-values in abstract, maybe strictly more than 3, 4, 5...

Example of many p-values: RESULTS: Mean follow-up duration was 23.1 ±
14.6 months. Postoperatively, visual acuity (LogMAR) improved
significantly from 1.34 ± 0.82 to 0.65 ± 0.79 (P \< 0.0001).
Postoperative complications included persistent vitreous hemorrhage
(15%) and neovascular glaucoma (4%). Final retinal reattachment rate was
97%. Preoperatively, macular detachment (P \< 0.0001) and Grade IV TRD
(P \< 0.0001) severity were significantly associated with poor final
best corrected visual acuity (P \< 0.0001). Preoperative macular
detachment (P \< 0.0001), Grade IV TRD (P \< 0.0001), intraoperative
iatrogenic breaks (P = 0.031), and postoperative neovascular glaucoma (P
\< 0.0001) were identified as significant predictors of poor
postoperative visual outcomes through multivariate analysis. CONCLUSION:
This study highlights the efficacy of 27 g PPV in improving visual
acuity in patients with diabetic TRD. Despite favorable outcomes,
attention to preoperative risk factors and meticulous surgical
techniques remain crucial for optimizing long-term visual prognosis in
these patients.

## Flowchart of studies in PubMed

-   What fraction of studies reports a p-value?

If something reports significant p-value, - is it a prospective or
retrospective study? - is it confirmatory of hypothesis generation? -
are the results in fact positive? (p-value can be difference between two
groups)

There should be a bit about men's height on dating app. Look, everyone
is at least 6ft!

## Refitting of meta-analyses in all possible datasets

Would be fun to measure heterogeneity

## add `topics` to clinical trial registries

The ClinicalTrials.gov and EUCTR `topic` columns are currently missing.
Preserve current rows, trial identifiers, effects, statistics and phase
`subset` values when adding topics. Save local copies of the relevant
processed dataset and `BEAR.rds` immediately before each change.

### ClinicalTrials.gov

In September 2026, the retained sample had 60,470 estimates from
23,060 trials. `domain_primary` was unknown for 7,742 estimates
(12.80%) and 2,442 trials (10.59%). These historical counts must be
recalculated for the current extract. The manual-review files under
`data_raw/clinicaltrials.gov/validation/manual_review/` provide
sampled unknown and multi-domain trials for inspection.

This audit reproduces the coverage breakdown without depending on a
saved CSV. Its filter follows `workflow/build_bear.R`; check that the
filter still agrees with the build before interpreting the result.

```r
library(dplyr)

clinical <- readRDS("data/clinicaltrialsgov.rds") %>%
  filter(include_in_bear, study_type == "INTERVENTIONAL",
         allocation == "RANDOMIZED", n_effect_rows_per_study < 20,
         !is.na(z))
bear <- readRDS("BEAR.rds")
stopifnot(nrow(clinical) == sum(bear$dataset == "clinicaltrials"))

coverage <- clinical %>%
  count(domain_primary, name = "estimates") %>%
  left_join(
    clinical %>% distinct(nct_id, domain_primary) %>%
      count(domain_primary, name = "trials"),
    by = "domain_primary"
  ) %>%
  arrange(desc(estimates))
print(coverage)

clinical %>%
  summarise(estimates = n(), trials = n_distinct(nct_id),
            unknown_estimates = sum(is.na(domain_primary) |
                                    domain_primary == "unknown"),
            unknown_trials = n_distinct(nct_id[is.na(domain_primary) |
                                                   domain_primary == "unknown"])) %>%
  mutate(unknown_estimate_pct = 100 * unknown_estimates / estimates,
         unknown_trial_pct = 100 * unknown_trials / trials)
```

The classifier in
`process/clinicaltrials.gov/process/05_build_trial_characteristics.R`
uses MeSH browse conditions and condition/keyword fallback, then selects
one domain by priority. Its richer output has `domain_primary`,
`domain_all`, `domain_n` and `domain_source`. Review three questionable
primary assignments: NCT02781727 (growth hormone deficiency) is
neurology; NCT04359771 (diabetic macular oedema) and NCT01056198
(diabetic foot wounds) are cardiovascular. Unknowns include healthy
volunteers and recognisable clinical subjects. A priority can select
a co-occurring condition instead of the principal clinical subject.

1.  Trace those examples and sampled unknown/multi-domain trials to
    matched terms and priorities in
    `process/clinicaltrials.gov/lib/domain_mesh_map.csv` and the
    manual-review files. Decide with the maintainer whether to publish
    the current priority-selected result or improve primary-domain
    selection. Reuse the existing classifier.
2.  Refresh trial metadata and join it to existing effects. Verify one
    characteristics row per `nct_id`, unchanged row count and order,
    and identical identifiers, effects and statistics against the
    saved processed dataset. Report estimates and distinct trials
    by domain, unknown rates and the three examples above.
3.  Once the domain rule is accepted, set `topic = domain_primary`
    in `workflow/build_bear.R`. Keep phase as `subset` and the richer
    domain fields in `data/clinicaltrialsgov.rds`. Document the source
    terms, fallback and priority rule. Validate topic categories and
    coverage. Compare the new `BEAR.rds` with its saved copy: same row
    order and all existing values except the intended `topic` change.

### EUCTR

The local database is `data_raw/eutrials/data/euctr_trials.sqlite`,
collection `euctr`. The processed file has no therapeutic-area field.
E.1.1.2 Therapeutic area is a candidate source; its database path and
representation are unverified. An earlier `ctrdata`/`nodbi`
installation failed because `jqr` needed `libjq-dev`. Resolve that
dependency before field discovery.

1.  Use `ctrShowOneTrial()` and `dbFindFields()` on the database to
    locate E.1.1.2 and inspect actual records. Record the observed
    field path and examples rather than inferring the JSON path.
2.  Extract metadata without redownloading trials or recomputing
    effects. Join by trial ID to `data/euctr.rds`; check for duplicate
    country records, unchanged row count/order, and identical
    identifiers, effects and statistics against the saved copy.
3.  Assess coverage in the exact retained `BEAR.rds` sample (8,650
    estimates in September 2026), by estimate and distinct trial.
    Tabulate categories, value types, country-record consistency,
    and multiple or hierarchical values.
4.  Keep the raw field in `data/euctr.rds`. Derive readable labels
    after inspecting codes, prefixes and multiple values. For example,
    prefer `Nervous System Diseases` to
    `Diseases [C] - Nervous System Diseases [C10]`.
5.  If coverage is poor, inspect E.1.1 medical condition and E.1.2
    MedDRA, and report their coverage before designing a fallback.
6.  Add the reviewed topic in `workflow/build_bear.R`, retain phase
    as `subset`, and document the exact source and derivation.
    Compare the new `BEAR.rds` with its saved copy: same row order and
    all existing values except the intended `topic` change. Report
    coverage and categories by estimate and trial.

### Making BEAR more data rich

What can we do? Can we add some columns? Subset columns? Can I Identify
overlapping sets in Arel-Bundock and Askarov
