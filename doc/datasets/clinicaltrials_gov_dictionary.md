## Data dictionary

The summary column is computed from `clinicaltrialsgov.rds` when this dictionary is regenerated. Counts are row-level effect counts, not unique trial counts.

### Row and source identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `effect_id` | Unique row identifier for the merged ClinicalTrials.gov effect row. |  |
| `author_effect_id` | Identifier for the author-reported outcome-analysis effect row. |  |
| `raw_effect_id` | Identifier for the selected raw endpoint calculation row. |  |
| `effect_source` | Source of the effect estimate used for b, se, z, and z_operator. | raw_derived 81,321 (53.8%); author_reported 69,927 (46.2%) |
| `include_in_bear` | Whether this row is used by the main BEAR ClinicalTrials.gov build. TRUE keeps author-reported rows and raw-derived rows only when no author-reported overlap is available. | TRUE 132,039 (87.3%) |
| `author_raw_overlap_key` | Shared key for linking author-reported rows to raw-derived rows for the same study, outcome, and result-group pair. |  |
| `nct_id` | ClinicalTrials.gov study identifier. |  |
| `outcome_id` | AACT outcome identifier linked to the effect row. |  |
| `outcome_analysis_id` | AACT outcome-analysis identifier for source-reported analyses. |  |

### Study characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `brief_title` | Brief trial title. |  |
| `official_title` | Official trial title. |  |
| `study_type` | ClinicalTrials.gov study type. | INTERVENTIONAL 144,998 (95.9%); OBSERVATIONAL 6,250 (4.1%) |
| `phase` | Trial phase as recorded by ClinicalTrials.gov. | PHASE2 42,281 (28.0%); missing 41,410 (27.4%); PHASE3 29,239 (19.3%); PHASE1 19,447 (12.9%); PHASE4 11,433 (7.6%); remaining 7,438 (4.9%) |
| `overall_status` | ClinicalTrials.gov recruitment or completion status. | completed 151,248 (100.0%) |
| `year` | Year used by BEAR for time summaries. | median 2,016 [2,012, 2,020]; range 1,992-2,025 |
| `enrollment` | Trial enrollment count. | median 122 [50, 400]; range 1-4,052,231; missing 2 |
| `enrollment_type` | Whether enrollment is actual or anticipated. |  |
| `number_of_arms` | Number of intervention arms recorded in the ClinicalTrials.gov study design table. | median 2 [2, 4]; range 1-32; missing 6,751 |
| `number_of_groups` | Number of observational groups recorded in the ClinicalTrials.gov study design table; usually missing for interventional trials. | median 1 [1, 2]; range 1-49; missing 145,361 |
| `results_first_posted_year` | Calendar year when results were first posted. |  |
| `primary_completion_year` | Calendar year of primary completion. |  |
| `n_effect_rows_per_study` | Number of public candidate effect rows linked to the same ClinicalTrials.gov study. | median 2 [1, 4]; range 1-2,474 |
| `n_outcomes_per_study` | Number of distinct outcome IDs represented for the same ClinicalTrials.gov study in the public file. | median 1 [1, 2]; range 1-96 |
| `allocation` | Allocation design recorded for the study. The main BEAR ClinicalTrials.gov dataset keeps randomized rows; non-randomized and missing-allocation rows remain identifiable in the public file. | RANDOMIZED 134,809 (89.1%); missing 11,178 (7.4%); NON_RANDOMIZED 5,261 (3.5%) |
| `intervention_model` | Intervention model recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on intervention model, although the raw-derived endpoint path is restricted to eligible randomized parallel interventional trials. | PARALLEL 126,424 (83.6%); CROSSOVER 9,364 (6.2%); SINGLE_GROUP 6,877 (4.5%); missing 6,452 (4.3%); FACTORIAL 1,422 (0.9%); remaining 709 (0.5%) |
| `primary_purpose` | Primary purpose recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on primary purpose. | TREATMENT 91,824 (60.7%); PREVENTION 28,223 (18.7%); missing 7,992 (5.3%); BASIC_SCIENCE 7,728 (5.1%); OTHER 6,179 (4.1%); remaining 9,302 (6.2%) |
| `observational_model` | Observational model if applicable; this is usually present only for observational studies and is therefore missing for most interventional rows. |  |
| `masking` | Masking design recorded for the study. | NONE 37,784 (25.0%); DOUBLE 36,855 (24.4%); QUADRUPLE 28,589 (18.9%); TRIPLE 25,955 (17.2%); SINGLE 15,376 (10.2%); remaining 6,689 (4.4%) |
| `is_factorial` | Whether the intervention model indicates a factorial design. | TRUE 1,422 (0.9%); missing 18 (0.0%) |
| `is_crossover` | Whether the intervention model indicates a crossover design. | TRUE 9,364 (6.2%); missing 18 (0.0%) |
| `n_design_groups` | Number of design-group records linked to the study. Unlike number_of_arms, this is counted from the design_groups table and is available for comparator summaries. | median 2 [2, 4]; range 0-49 |
| `has_placebo_comparator` | Whether any design-group record appears to be a placebo comparator. | TRUE 60,187 (39.8%) |
| `comparator_summary` | Compact study-level comparator summary derived from design-group labels and descriptions. |  |
| `is_multi_arm` | Whether the study has more than two arms or groups. | TRUE 70,817 (46.8%) |
| `has_baseline_measurements` | Whether baseline measurement rows were available in AACT. | TRUE 151,228 (100.0%) |

### Trial categories

| Variable | Definition | Summary |
|:--|:--|:--|
| `domain_all` | All mapped clinical domains for the trial. |  |
| `domain_n` | Number of mapped clinical domains for the trial. |  |
| `domain_source` | Source used for domain mapping. |  |
| `domain_oncology` | Whether the trial maps to oncology. | TRUE 10,490 (6.9%) |
| `domain_primary` | Primary mapped clinical domain. | infectious_disease 30,432 (20.1%); unknown 19,334 (12.8%); mental_health 16,643 (11.0%); endocrine_metabolic 13,645 (9.0%); cardiovascular 11,580 (7.7%); remaining 59,614 (39.4%) |
| `intervention_types_all` | All intervention types recorded for the study. |  |
| `intervention_type_n` | Number of intervention types recorded for the study. |  |
| `intervention_type_primary` | Primary intervention type used for summaries. | drug 91,321 (60.4%); biological 21,442 (14.2%); behavioral 14,414 (9.5%); device 11,062 (7.3%); other 7,017 (4.6%); remaining 5,992 (4.0%) |
| `is_drug_trial` | Whether any intervention is coded as drug. | TRUE 102,475 (67.8%) |
| `is_biological_trial` | Whether any intervention is coded as biological. | TRUE 31,618 (20.9%) |
| `is_device_trial` | Whether any intervention is coded as device. | TRUE 12,300 (8.1%) |
| `is_procedure_trial` | Whether any intervention is coded as procedure. | TRUE 3,582 (2.4%) |
| `is_behavioral_trial` | Whether any intervention is coded as behavioral. | TRUE 16,773 (11.1%) |
| `is_vaccine_trial` | Whether intervention text indicates a vaccine trial. | TRUE 17,863 (11.8%) |
| `is_cell_or_gene_therapy_trial` | Whether intervention text indicates cell or gene therapy. | TRUE 1,029 (0.7%) |
| `is_diagnostic_trial` | Whether any intervention is coded as diagnostic test. | TRUE 5,579 (3.7%) |
| `lead_sponsor_name` | Lead sponsor name. |  |
| `lead_sponsor_class` | Lead sponsor class. | industry 98,759 (65.3%); other 44,847 (29.7%); other_gov 3,958 (2.6%); nih 3,027 (2.0%); network 657 (0.4%) |
| `has_industry_sponsor` | Whether the lead sponsor class is industry. | TRUE 105,229 (69.6%) |

### Outcome descriptors

| Variable | Definition | Summary |
|:--|:--|:--|
| `outcome_type` | Outcome type recorded in ClinicalTrials.gov. | PRIMARY 151,248 (100.0%) |
| `outcome_title` | Outcome title. |  |
| `outcome_time_frame` | Outcome time frame. |  |
| `measurement_stratum_id` | AACT measurement stratum identifier for raw-derived rows. |  |
| `measurement_title` | Measurement title for raw-derived endpoint rows. |  |
| `measurement_units` | Measurement units for raw-derived endpoint rows. |  |

### BEAR effect fields

| Variable | Definition | Summary |
|:--|:--|:--|
| `measure_class` | Effect-size family used by BEAR summaries. | Standardized Mean Difference 49,955 (33.0%); Probit Difference 31,366 (20.7%); missing 24,211 (16.0%); Mean Difference 17,587 (11.6%); Geometric Ratio 7,062 (4.7%); remaining 21,067 (13.9%) |
| `measure_detailed` | Detailed effect-size label distinguishing Hedges g for raw-derived standardized mean differences from author-reported SMD. |  |
| `scale` | Scale of the stored effect size. | raw 53,035 (35.1%); smd 49,955 (33.0%); probit 31,366 (20.7%); log 16,892 (11.2%) |
| `effect` | Effect estimate used for BEAR-facing analysis. |  |
| `b` | Effect estimate used for z-value calculations where available. |  |
| `se` | Standard error of the effect estimate where available. |  |
| `z` | Signed z-value used in BEAR analyses. |  |
| `z_operator` | Operator describing whether z is exact or bounded. | = 143,517 (94.9%); > 6,927 (4.6%); < 804 (0.5%) |

### Author-reported analysis inputs

| Variable | Definition | Summary |
|:--|:--|:--|
| `linked_result_group_ids` | Result-group identifiers linked to an author-reported outcome analysis. |  |
| `linked_group_codes` | Group codes linked to an author-reported outcome analysis. |  |
| `n_linked_groups` | Number of result groups linked to an author-reported analysis. |  |
| `method` | Statistical method reported for the outcome analysis. |  |
| `param_type` | Parameter type reported for the outcome analysis. |  |
| `estimate` | Source-reported effect estimate. |  |
| `lower` | Source-reported confidence interval lower bound. |  |
| `upper` | Source-reported confidence interval upper bound. |  |
| `p_value` | Source-reported p-value. |  |
| `p_value_modifier` | Source-reported p-value inequality modifier. |  |
| `ci_percent` | Confidence level of the source-reported interval. |  |
| `ci_n_sides` | Number of sides for the source-reported confidence interval. |  |

### Raw endpoint calculation inputs

| Variable | Definition | Summary |
|:--|:--|:--|
| `raw_event_t` | Event count in the treatment or focal group for binary raw contrasts. |  |
| `raw_event_c` | Event count in the comparator group for binary raw contrasts. |  |
| `raw_n_t` | Analysis count in the treatment or focal group for raw contrasts. |  |
| `raw_n_c` | Analysis count in the comparator group for raw contrasts. |  |
| `raw_mean_t` | Mean in the treatment or focal group for continuous raw contrasts. |  |
| `raw_mean_c` | Mean in the comparator group for continuous raw contrasts. |  |
| `raw_sd_t` | Standard deviation in the treatment or focal group for continuous raw contrasts. |  |
| `raw_sd_c` | Standard deviation in the comparator group for continuous raw contrasts. |  |

### Raw result-group metadata

| Variable | Definition | Summary |
|:--|:--|:--|
| `raw_group_t_id` | AACT result-group identifier for the treatment or focal group. |  |
| `raw_group_c_id` | AACT result-group identifier for the comparator group. |  |
| `raw_group_t_title` | Title of the treatment or focal result group. |  |
| `raw_group_c_title` | Title of the comparator result group. |  |
| `direction_unknown` | Whether treatment-comparator direction could not be inferred for the raw-derived contrast. | TRUE 22,174 (14.7%); missing 69,927 (46.2%) |
| `raw_multi_arm_trial` | Whether the raw-derived contrast comes from a multi-arm trial. | TRUE 47,183 (31.2%); missing 69,927 (46.2%) |
