# Unknown source rows in ClinicalTrials.gov v2 endpoint extraction

This note documents a lean exploration of rows classified as `unknown` in
`data_raw/clinicaltrials.gov/derived/legacy_analysis_output/raw_source_rows_v2.rds`.

The exploration read the reduced v2 source-row audit, not the large raw AACT
flat file or the 645MB v2 candidate CSV. It used grouped counts and field-level
keyword scans over the 143,376 `unknown` rows.

## Summary

The `unknown` bucket is broad but not opaque. It is mostly conservative
non-inclusion rather than a set of obviously missed clean endpoint effects.

The 143,376 rows break down by original v2 candidate class as:

| source row candidate class | rows |
|---|---:|
| `binary_count_explicit` | 131,969 |
| `continuous_mean_sd` | 9,518 |
| `continuous_mean_se` | 1,001 |
| `continuous_mean_ci` | 605 |
| `continuous_lsm_se` | 239 |
| `continuous_lsm_ci` | 44 |

The continuous unknown rows are all baseline-only measurements under the
current baseline screen:

| continuous candidate class | rows |
|---|---:|
| `continuous_mean_sd` | 9,518 |
| `continuous_mean_se` | 1,001 |
| `continuous_mean_ci` | 605 |
| `continuous_lsm_se` | 239 |
| `continuous_lsm_ci` | 44 |

The binary count unknown rows are primarily `COUNT_OF_PARTICIPANTS` rows whose
category text is missing or does not identify a positive endpoint event cleanly
enough for the current rules. In particular, 134,241 of all unknown rows have
missing `category_norm`.

## Field-level patterns

Top `category_norm` values among unknown rows were mostly missing or
non-positive/non-event labels:

| category | rows |
|---|---:|
| missing | 134,241 |
| none | 915 |
| not reported | 642 |
| no | 322 |
| i don't know | 234 |
| agree | 110 |
| mild | 100 |
| severe | 99 |
| moderate | 96 |
| disagree | 87 |

The top measurement titles show several recurring families:

| recurring title pattern | examples |
|---|---|
| laboratory abnormalities | haematological and biochemical laboratory abnormalities; abnormal hematology/chemistry values; clinical laboratory test abnormalities |
| solicited symptoms/reactogenicity | solicited systemic symptoms; solicited local symptoms; reactogenicity signs and symptoms |
| adverse-event summaries | adverse events; treatment-emergent adverse events; serious adverse events |
| ordinal severity/category rows | grade 1/2/3; mild/moderate/severe; normal/abnormal |
| score/questionnaire categories | expectancy ratings; parent-child communication; questionnaire response categories |
| disposition/adherence | early discontinuation; reasons for discontinuation; adherence |
| demographics or baseline-like fields | baseline abnormalities, prior/history fields, fundus/lens opacity categories |

The top `measurement_units_norm` values also explain the split:

| units | rows |
|---|---:|
| participants | 131,969 |
| score on a scale | 2,247 |
| units on a scale | 2,084 |
| mmHg | 267 |
| scores on a scale | 139 |

The non-participant units are mostly from continuous baseline-only rows. The
participant-unit rows are usually count rows that need stronger event
interpretation before they can be turned into binary endpoints.

## Approximate review buckets

A rough, overlapping text scan over explicit row fields
(`measurement_title_norm`, `measurement_description_norm`, `outcome_title`,
and `category_norm`) found these broad signals among unknown rows:

| signal | rows |
|---|---:|
| severity/category language | 106,537 |
| demographic/baseline language | 63,365 |
| laboratory-abnormality language | 58,587 |
| adverse-event/reactogenicity language | 56,999 |
| score/scale/questionnaire language | 30,924 |
| disposition/adherence language | 9,683 |

These counts overlap. They should be read as descriptive signals, not as a
partition. For example, a row about grade 3 laboratory abnormalities can match
both severity and laboratory-abnormality patterns.

A non-overlapping first-pass bucket assignment gave:

| first-pass review bucket | rows |
|---|---:|
| sparse or generic participant-count rows | 122,834 |
| continuous baseline-only | 11,407 |
| named categories not event-like | 4,224 |
| adverse events or reactogenicity | 1,387 |
| ordinal or severity categories | 1,293 |
| score or scale categories | 1,029 |
| demographic or baseline-like | 843 |
| timepoint or visit categories | 272 |
| treatment or concomitant categories | 46 |
| laboratory abnormalities | 35 |
| possibly non-participant or unit categories | 6 |

The large `sparse_or_generic_count_rows` bucket is a limitation of forcing a
non-overlapping taxonomy on sparse registry rows. Many of those rows have
missing `category_norm`, but their titles still contain laboratory,
reactogenicity, adverse-event, severity, or demographic language.

## Interpretation

The `unknown` category mostly contains rows where a participant count exists
but the row cannot be safely interpreted as a positive binary endpoint under
the current conservative rules.

The most common situations are:

1. **Missing category labels.** Many rows have participant counts and
   event-like titles but no category field. Without a positive category,
   complement category, or denominator-valid percent/proportion rule, the
   current classifier leaves them unknown.
2. **Safety/reactogenicity-like endpoints.** Many titles concern adverse
   events, solicited local/systemic symptoms, reactogenicity, lab
   abnormalities, and toxicity grades. These may be legitimate endpoint rows in
   some trials, but the current project explicitly keeps safety-total style
   extraction out of scope and avoids broad inclusion of such rows.
3. **Ordinal or multi-category summaries.** Rows with grade, severity, normal
   versus abnormal, response categories, or questionnaire responses often need
   category-specific decisions. Some could be converted if a target event
   category were defined, but many are not a single binary event.
4. **Baseline-only continuous rows.** All continuous unknown rows were
   baseline-only under the current screen and were deliberately excluded from
   endpoint effects.
5. **Scale or score categories.** Some count rows describe categories of
   questionnaire/scale responses rather than endpoint events.
6. **Disposition/adherence rows.** Some rows count withdrawal,
   discontinuation, adherence, or completion categories. These are not
   automatically endpoint effects.

## Possible future classifier work

If the goal is to recover more effects, the highest-yield next step is not to
globally include `unknown`; it is to split this bucket into explicit additional
classes:

- `excluded_safety_or_reactogenicity`
- `excluded_laboratory_abnormality`
- `excluded_ordinal_or_multicategory`
- `excluded_disposition_or_adherence`
- `excluded_score_response_category`
- `excluded_missing_positive_category`

For actual effect recovery, a conservative extension could target only rows
with participant denominators, event-like titles, and no evidence that the row
is safety/reactogenicity, baseline, ordinal severity, or disposition. The
largest apparent opportunity is better handling of missing-category
participant-count rows, but that would need manual review because many are lab
or adverse-event summaries.
