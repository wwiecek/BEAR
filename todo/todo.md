# To-do items for BEAR



## Add roadmap and additions on the website

People should be able to see what's coming next
This to-do should partly be turned into description of issues for the website
People should be able to submit their own data and flag issues



### Zero or non-positive bounds for ratio confidence intervals

Barnett--Wren previously replaced zero or negative lower bounds with a small
positive constant before taking logarithms. Clinical-trial registries instead
derive a CI-based z-value only when the estimate and both bounds are strictly
positive; otherwise they use a p-derived z-value if one is available, or drop
the row from `BEAR.rds`. Raw ClinicalTrials.gov log-ratio estimands with
boundary cells are also excluded rather than continuity-corrected.

We should decide whether any future ratio-CI workflow needs a documented
continuity-correction policy. Until then, retain the source values in individual
datasets and do not add an arbitrary positive constant when constructing
`BEAR.rds`.



## DOI additions and studyid fixes

Once DOIs are available, we should include DOI column in BEAR

In Bartos.Rmd we say

> - **Study ID:** we use `id` from original data, which is unique to each row and cannot group estimates within studies.

Could this not be fixed? I assume there will be multiple estimates per paper. Or 
are we positive this is genuinely one paper, one effect?





Look at this old task:

## Replace studyid with DOI in as many datasets as possible

We tried doing it for Lang paper (see process/ script lookup dois), but what about going dataset by dataset and seeing where available data could be used to track down DOIs for individual studies?

Start by summarising what studyid is in each dataset
Catalogue possible alternative choices already present in each dataset
Then say whether you think it would be possible to search for DOIs based on data you have

Do not edit any existing scripts for now. Investigation only.


Assessment results:

  The current studyid mapping comes from workflow/build_bear.R. High-level catalogue:

  | Dataset | Current studyid | Alternatives already present | DOI lookup feasibility |
  |---|---|---|---|
  | Head | pmid | first.doi, journal, year | Already has article DOI. Easiest replacement. |
  | SCORE_claims | DOI | paper/claim IDs, citation | Already DOI. |
  | SCORE_replications | DOI | paper/claim/report IDs, citation | Already DOI. |
  | Lang | synthetic Lang_paper_* | source_title, citation, journal, year, paper IDs | Good. Existing Crossref script uses exactly these fields, but current data/Lang.rds does not
  show a doi column. |
  | Brodeur | publication title | doinumber, RCT IDs, publication title/authors/journal/year, some DOI URLs in relevantpapersforcsv | Good but needs care. doinumber is mainly AEA
  RCT registry DOI, not necessarily article DOI. Article-title Crossref lookup should work for many. |
  | JagerLeek | PubMed ID | title, year | Good. PMID-to-DOI via PubMed/Europe PMC likely straightforward. |
  | BarnettWren | PubMed ID | journal, year, source | Good. PMID-to-DOI lookup should work, though many PMIDs. |
  | Chavalarias | likely PubMed-style article ID | journal-ish ID, year, source | Probably good if studyid is PMID. Need confirm from source docs; then PMID-to-DOI. |
  | clinicaltrials | nct_id | brief title, phase/year | Not really DOI. Registry IDs are better identifiers than DOIs; trial-result publications would require external linking. |
  | euctr | EUCTR trial ID | endpoint title, phase/date | Not really DOI. Trial registry identifier should remain. |
  | Cochrane | study.name | study.id, study year, review DOI/id | Weak from processed data. The DOI present is Cochrane review DOI, not individual study DOI. Study author-year
  strings are ambiguous. |
  | ArelBundock | author/year-ish study_id | meta ID, study year, journal, question ID | Medium/weak. Could Crossref-search author/year/journal, but no title; false matches likely.
  |
  | WWC | internal study_id | StudyID, full Citation, year | Good. Citation strings are rich enough for Crossref. |
  | Yang | author-year study_ID | meta ID, publication year | Medium. Author-year only; possible but ambiguous without titles. |
  | CostelloFox | cleaned author-year study2 | raw study, meta-analysis IDs, study year | Medium/weak. Similar author-year ambiguity. |
  | Metapsy | author-year study | metaid, year | Medium. Could search author/year plus meta-analysis context, but needs validation. |
  | OSC | replication project study number | original title, authors, journal | Good. Original article metadata is present; Crossref should work. |
  | ManyLabs2 | site/source filename | metaid, source | No. These are replication-site/sample identifiers, not source articles. |
  | psymetadata | numeric/internal study ID | metaid, subset, year | Weak. Not enough citation metadata in processed file. |
  | Nuijten | numeric/internal study ID | metaid, year | Weak. Same issue as psymetadata. |
  | Sladekova | row number assigned in build | metaid file name, year | Very weak from processed data. Current studyid is artificial. |
  | Bartos | row-level id | reference, author, year, meta_id | Partial. reference can give DOI for the meta-analysis/source paper, but current rows are not clearly individual study
  articles. |
  | Askarov | numeric studyid | journal, year/submission year, filename | Weak. No title or author in processed data. |
  | ArelBundock note | study_id has invalid multibyte text in at least one value |  | Needs encoding cleanup before robust string matching. |

  Best candidates for a first DOI-enrichment pass: Head, SCORE_*, Lang, JagerLeek, BarnettWren, Chavalarias, WWC, OSC, and probably Brodeur.

  I would not try to force DOI replacement for trial-register datasets (clinicaltrials, euctr) or sample-level replication datasets (ManyLabs2) because their current identifiers are
  more meaningful than article DOIs.
  
  
  
  
  
  


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

Would we be able to code them as topics or article titles by
looking up each article?


## Remove source variable.

It doesn't seem to add any information, I think we could simply remove it? 
Investigate why it exists and give me your assessment first.
Are there other columns in BEAR that could be removed already?



## Documentation should also include description of the model


## Lang fixes

REview data_raw/Lang/ in detail. Is there a trace to the authors picking one hypothesis
per study systematically (focal hypothesis) or we cannot distinguish them?
When they do one-observation-per-paper does it look like they grabbed it at random or systematically?
YOu may need to read the original paper for context.
Should BEAR.rds be picking one estimate per each MHT group? Read their documentatioin
and code and give your opinion. Would our results change if we did that?

In Lang.rds all `source_` variables, save for MHT, do not feel useful to the 
consumers of BEAR. Do you agree? If yes, remove. 

Some checking and validation code seems pointless here given that we only do this 
data processing once, like you shouldn't check for required columns. See if this 
script can be simplified while still producing exactly the same results we had 
to date

Use journal as `topic`? Could use JEL as a topic variable, but maybe there is an 
easier classification here?



                       
## Small dataset-specific checks

### Issue: Sladekova study IDs and `abs(yi) > 1` brief

Current status: `process/Sladekova.R` already reconstructs study IDs from DOI
or descriptive fields where possible. In the current processed file, 9,748 of
11,591 rows use a DOI or descriptive label; 1,843 rows (15.9%) still fall back
to row-unique IDs because no descriptive study label was available after
processing.

Short brief for Bartos: BEAR treats the Sladekova `yi` values as
correlation-scale effects and applies a Fisher z transform. In the local source
files I find 43 rows from 10 files with `abs(yi) > 1`, mainly `B109_1.csv`
(20 rows) and `A23_1.csv`/`A23_2.csv` (7 rows each). These values cannot be
ordinary correlations, so I need to know whether they are coding errors, a
different effect-size scale in those files, or rows that should be excluded
before applying the correlation-scale transform.

Potential follow-up after reply: if these rows are invalid correlations, update
`process/Sladekova.R` to drop or separately classify them before the Fisher
transform, then document the row count change in `doc/datasets/Sladekova.Rmd`.


## Data dictionaries for richer public datasets

Short spec:

1. Prioritise datasets with many columns or substantial metadata beyond
   `BEAR.rds`: ClinicalTrials.gov, Brodeur, Askarov, OSC, SCORE, WWC,
   Cochrane, Lang, EUCTR, and Bartos.
2. For each dataset, inspect the saved public `.rds` and make a best-guess
   definition for every column: row unit, source, meaning, allowed values or
   units, missingness, and notes needed for interpretation.
3. Check source documentation, package docs, replication files, papers, and
   online data dictionaries where available. Replace guesses with sourced
   definitions and record unresolved ambiguity.
4. Reconsider the public `data/` schema before finalising the dictionary:
   decide which extra columns should remain in the full public dataset, which
   should be renamed or collapsed, and which are internal audit fields that
   should stay out of `data/`.
5. Store each dictionary beside the dataset docs as
   `doc/datasets/<dataset>_dictionary.csv`, with a reproducible
   `doc/datasets/build_<dataset>_dictionary.R` script that validates one
   dictionary row per public column and writes
   `doc/datasets/<dataset>_dictionary.md` for the website.
6. As this workflow matures, keep generalising the rules in
   `doc/adding_new_datasets.md` so future datasets follow the same pattern.

For clinicaltrials.gov dataset in data/
Prepare an exploration (new script, concise!)
of how effect sizes differ according to crucial predictors

- author-reported vs raw-outcome-derived
- phase
- domain
- type of trial (drug, biological, device, procedure etc)
- measure
- ???



I am interested in proportion significant, median |z|, median |z| among significant 
results other simple summaries in that category.

Also write code from explore/subgroups/ to fit mixture model to the crucial subsets
of data you identify and some plotting code to compare them (plot mixtures and repl vs omega).
And a readable table summarising fitting results (power, sign, replication Pr).
Do not run it but leave it for me to run.

You could also do an additional comparison for studies where there are pairs of 
 author-reported
effect size (which we currently use) and raw-outcome-derived effect size (which we currentrly ignore
if there is author-reported alternative). For that you'd also do some exploratory scatter plots and summary tables, not just fitting the mixture.


## ctgov variables


You could also group baseline measurement columns (has_baseline_measurement and raw data) into a separate category in presenting data dictionary

Why is p-value sides always 2? That seems wrong. In most cases I bet it's not even reported.
You could also just remove it from user-facing file if it's always 2

aren't scale and measure_class and raw_measure redundant with each other? wouldn't it be better to create a single categorical variable here? how are they used in build_bear?

derivation_rule_id seems like an internal variable do not retain for data/

Most of "Raw result-group metadata" does not seem very relevant/useful to an end user.
Please review and consider which variables to retain. Feel free to disagree.




  
## PubMed downloads

Can we get:

- Number of citations
- Article title
- Journal
- Journal grouping into disciplines (a la Head 2015 grouping of journals)
- Classification codes (like JEL)
- Type of experiment, prospective vs retrospective, randomised, controlled, 

Borrow Barnett and Wren approaches

Quantify in what % of articles we have odds ratios, risk ratios, p-values reported

Do publication bias assessment only on articles that have 1 p-value in the abstract
then compare it to an assessment where we have multiple p-values in abstract, maybe
strictly more than 3, 4, 5...

Example of many p-values:
RESULTS: Mean follow-up duration was 23.1 ± 14.6 months. Postoperatively, visual acuity (LogMAR) improved significantly from 1.34 ± 0.82 to 0.65 ± 0.79 (P < 0.0001). Postoperative complications included persistent vitreous hemorrhage (15%) and neovascular glaucoma (4%). Final retinal reattachment rate was 97%. Preoperatively, macular detachment (P < 0.0001) and Grade IV TRD (P < 0.0001) severity were significantly associated with poor final best corrected visual acuity (P < 0.0001). Preoperative macular detachment (P < 0.0001), Grade IV TRD (P < 0.0001), intraoperative iatrogenic breaks (P = 0.031), and postoperative neovascular glaucoma (P < 0.0001)
were identified as significant predictors of poor postoperative visual outcomes through multivariate analysis.	CONCLUSION: This study highlights the efficacy of 27 g PPV in improving visual acuity in patients with diabetic TRD. Despite favorable outcomes, attention to preoperative risk factors and meticulous surgical techniques remain crucial for optimizing long-term visual prognosis in these patients.



## Flowchart of studies in PubMed

- What fraction of studies reports a p-value?

If something reports significant p-value, 
- is it a prospective or retrospective study?
- is it confirmatory of hypothesis generation?
- are the results in fact positive? (p-value can be difference between two groups)

There should be a bit about men's height on dating app. Look, everyone is at least 6ft!


## Refitting of meta-analyses in all possible datasets

Would be fun to measure heterogeneity





## add `topics` to clinical trial registries

Currently in the clinical trial registry datasets,
`topic` columns remain missing pending the work below. Preserve all current
rows, identifiers, statistics and trial-phase subsets.

### ClinicalTrials.gov

The exact retained sample has 60,470 estimates from 23,060 trials. Unknown domain
accounts for 7,742 estimates (12.80%) and 2,442 trials (10.59%). Category counts
are in `data_raw/v3_extensions/clinical_topic_coverage.csv`, reproduced by the
local `audit_baseline.R`. The existing manual-review files contain 130 unknown
and 141 multi-domain trials in this sample.

Inspection exposed problematic primary-domain assignments: NCT02781727 (growth
hormone deficiency) is neurology; NCT04359771 (diabetic macular oedema) and
NCT01056198 (diabetic foot wounds) are cardiovascular. Unknowns include both
healthy-volunteer studies and recognisable clinical subjects. The priority rules
can select a co-occurring condition rather than the principal clinical subject.

1. Review `process/clinicaltrials.gov/process/05_build_trial_characteristics.R`
   and its existing MeSH/condition/keyword mappings. Trace these examples to
   actual matched terms and priorities, using the review CSVs under
   `data_raw/clinicaltrials.gov/validation/manual_review/`.
2. Obtain a maintainer decision on publishing the current result explicitly as
   a priority-selected matched domain versus improving the primary-domain rules.
   Reuse the existing classifier; do not introduce a competing taxonomy.
3. Preserve `domain_all`, `domain_n` and `domain_source` in the richer dataset.
   Refresh only metadata and compare every effect/identifier/statistic with the
   baseline. Report trials and estimates by domain, unknown rates and spot checks.
4. Once reviewed, assign `topic = domain_primary` centrally. Document MeSH browse
   conditions, condition/keyword fallback and multi-domain priorities. Keep phase
   as `subset`; extend the validation checks to verify coverage and categories.

### EUCTR

The local database is `data_raw/eutrials/data/euctr_trials.sqlite`, collection
`euctr`. The processed file lacks therapeutic area. Its field path has not been
verified. `ctrdata` and `nodbi` installation failed at `jqr` because `libjq-dev`
is absent; sudo requires a password. Temporary partial installations under
`/tmp/bear-v3-Rlib` are not a durable environment setup.

1. Install the dependencies and use `ctrShowOneTrial()` / `dbFindFields()` on the
   existing database to discover E.1.1.2 Therapeutic area's actual path. Record
   observed examples; do not guess the JSON name.
2. Extract metadata without redownloading or recomputing endpoint effects. Verify
   trial joins preserve row count/order and do not duplicate country records.
3. Report coverage among the exact retained BEAR sample (currently 8,650
   estimates), both by estimate and distinct trial; tabulate categories, value
   types, country-record consistency and multiple/hierarchical values.
4. Preserve the raw registry field in `data/euctr.rds`. Derive readable labels
   only after reviewing representations, hierarchy prefixes, codes and multiple
   values. Prefer Nervous System Diseases to Diseases [C] - Nervous System
   Diseases [C10].
5. If coverage is poor, investigate E.1.1 medical condition and E.1.2 MedDRA and
   report findings before designing a fallback classifier.
6. Add the reviewed topic centrally, document its E.1.1.2 source, retain phase
   as `subset`, and verify coverage plus complete statistical/row invariance.




### Making BEAR more data rich

What can we do?
Can we add some columns?
Subset columns?
Can I Identify overlapping sets in Arel-Bundock and Askarov 

