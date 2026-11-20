## Data dictionary

The summary column is computed from `clinicaltrialsgov.rds` when this dictionary is regenerated. Counts are row-level effect counts, not unique trial counts.

### Row and source identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `effect_id` | Unique row identifier for the merged ClinicalTrials.gov effect row. |  |
| `author_effect_id` | Identifier for the author-reported outcome-analysis effect row. |  |
| `raw_effect_id` | Identifier for the selected raw endpoint calculation row. |  |
| `effect_source` | Source of the effect estimate used for b, se, z, and z_operator. | raw_derived 88,633 (53.5%); author_reported 76,938 (46.5%) |
| `include_in_bear` | Selected by the author-versus-raw rule for possible inclusion in BEAR.rds. When both values are available for the same comparison, BEAR.rds uses the author-reported value; the raw-derived value remains available in this ClinicalTrials.gov dataset with this flag set to FALSE. | TRUE 144,436 (87.2%) |
| `result_id` | BEAR-defined identifier linking calculations for the same trial, outcome and unordered pair of result groups. |  |
| `nct_id` | ClinicalTrials.gov study identifier. |  |
| `outcome_id` | AACT outcome identifier linked to the effect row. |  |
| `outcome_analysis_id` | AACT outcome-analysis identifier for source-reported analyses. |  |

### Study characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `brief_title` | Brief trial title. |  |
| `official_title` | Official trial title. |  |
| `study_type` | ClinicalTrials.gov study type. | INTERVENTIONAL 158,853 (95.9%); OBSERVATIONAL 6,718 (4.1%) |
| `phase` | Trial phase as recorded by ClinicalTrials.gov. | missing 49,222 (29.7%); PHASE2 44,408 (26.8%); PHASE3 30,925 (18.7%); PHASE1 20,280 (12.2%); PHASE4 12,209 (7.4%); remaining 8,527 (5.2%) |
| `overall_status` | ClinicalTrials.gov recruitment or completion status. | completed 165,571 (100.0%) |
| `year` | Year used by BEAR for time summaries. | median 2,017 [2,012, 2,021]; range 1,992-2,026 |
| `enrollment` | Trial enrollment count. | median 124 [50, 400]; range 1-4,052,231; missing 2 |
| `enrollment_type` | Whether enrollment is actual or anticipated. |  |
| `number_of_arms` | Number of intervention arms recorded in the ClinicalTrials.gov study design table. | median 2 [2, 4]; range 1-32; missing 7,220 |
| `number_of_groups` | Number of observational groups recorded in the ClinicalTrials.gov study design table; usually missing for interventional trials. | median 1 [1, 2]; range 1-49; missing 159,328 |
| `results_first_posted_year` | Calendar year when results were first posted. |  |
| `primary_completion_year` | Calendar year of primary completion. |  |
| `n_effect_rows_per_study` | Number of public candidate effect rows linked to the same ClinicalTrials.gov study. | median 2 [1, 4]; range 1-2,474 |
| `n_outcomes_per_study` | Number of distinct outcome IDs represented for the same ClinicalTrials.gov study in the public file. | median 1 [1, 2]; range 1-96 |
| `allocation` | Allocation design recorded for the study. The main BEAR ClinicalTrials.gov dataset keeps randomized rows; non-randomized and missing-allocation rows remain identifiable in the public file. | RANDOMIZED 147,271 (88.9%); missing 12,505 (7.6%); NON_RANDOMIZED 5,795 (3.5%) |
| `intervention_model` | Intervention model recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on intervention model, although the raw-derived endpoint path is restricted to eligible randomized parallel interventional trials. | PARALLEL 138,046 (83.4%); CROSSOVER 10,104 (6.1%); SINGLE_GROUP 7,785 (4.7%); missing 6,926 (4.2%); FACTORIAL 1,702 (1.0%); remaining 1,008 (0.6%) |
| `primary_purpose` | Primary purpose recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on primary purpose. | TREATMENT 98,923 (59.7%); PREVENTION 31,281 (18.9%); BASIC_SCIENCE 8,565 (5.2%); missing 8,440 (5.1%); OTHER 7,424 (4.5%); remaining 10,938 (6.6%) |
| `observational_model` | Observational model if applicable; this is usually present only for observational studies and is therefore missing for most interventional rows. |  |
| `masking` | Masking design recorded for the study. | NONE 42,545 (25.7%); DOUBLE 39,009 (23.6%); QUADRUPLE 30,911 (18.7%); TRIPLE 27,830 (16.8%); SINGLE 18,117 (10.9%); remaining 7,159 (4.3%) |
| `is_factorial` | Whether the intervention model indicates a factorial design. | TRUE 1,702 (1.0%); missing 18 (0.0%) |
| `is_crossover` | Whether the intervention model indicates a crossover design. | TRUE 10,104 (6.1%); missing 18 (0.0%) |
| `n_design_groups` | Number of design-group records linked to the study. Unlike number_of_arms, this is counted from the design_groups table and is available for comparator summaries. | median 2 [2, 4]; range 0-49 |
| `has_placebo_comparator` | Whether any design-group record appears to be a placebo comparator. | TRUE 64,144 (38.7%) |
| `comparator_summary` | Compact study-level comparator summary derived from design-group labels and descriptions. |  |
| `is_multi_arm` | Whether the study has more than two arms or groups. | TRUE 76,478 (46.2%) |
| `has_baseline_measurements` | Whether baseline measurement rows were available in AACT. | TRUE 165,549 (100.0%) |

### Trial categories

| Variable | Definition | Summary |
|:--|:--|:--|
| `domain_all` | All mapped clinical domains for the trial. |  |
| `domain_n` | Number of mapped clinical domains for the trial. |  |
| `domain_source` | Source used for domain mapping. |  |
| `domain_oncology` | Whether the trial maps to oncology. | TRUE 11,775 (7.1%) |
| `domain_primary` | Primary mapped clinical domain. | infectious_disease 32,210 (19.5%); unknown 22,906 (13.8%); mental_health 18,710 (11.3%); endocrine_metabolic 14,333 (8.7%); neurology 13,355 (8.1%); remaining 64,057 (38.7%) |
| `intervention_types_all` | All intervention types recorded for the study. |  |
| `intervention_type_n` | Number of intervention types recorded for the study. |  |
| `intervention_type_primary` | Primary intervention type used for summaries. | drug 96,123 (58.1%); biological 23,138 (14.0%); behavioral 18,389 (11.1%); device 12,770 (7.7%); other 8,550 (5.2%); remaining 6,601 (4.0%) |
| `is_drug_trial` | Whether any intervention is coded as drug. | TRUE 107,722 (65.1%) |
| `is_biological_trial` | Whether any intervention is coded as biological. | TRUE 31,844 (19.2%) |
| `is_device_trial` | Whether any intervention is coded as device. | TRUE 14,233 (8.6%) |
| `is_procedure_trial` | Whether any intervention is coded as procedure. | TRUE 3,836 (2.3%) |
| `is_behavioral_trial` | Whether any intervention is coded as behavioral. | TRUE 21,108 (12.7%) |
| `is_vaccine_trial` | Whether intervention text indicates a vaccine trial. | TRUE 19,441 (11.7%) |
| `is_cell_or_gene_therapy_trial` | Whether intervention text indicates cell or gene therapy. | TRUE 1,134 (0.7%) |
| `is_diagnostic_trial` | Whether any intervention is coded as diagnostic test. | TRUE 6,569 (4.0%) |
| `lead_sponsor_name` | Lead sponsor name. |  |
| `lead_sponsor_class` | Lead sponsor class. | industry 104,377 (63.0%); other 52,511 (31.7%); other_gov 4,656 (2.8%); nih 3,272 (2.0%); network 755 (0.5%) |
| `has_industry_sponsor` | Whether the lead sponsor class is industry. | TRUE 111,219 (67.2%) |

### Outcome descriptors

| Variable | Definition | Summary |
|:--|:--|:--|
| `outcome_type` | Outcome type recorded in ClinicalTrials.gov. | PRIMARY 165,571 (100.0%) |
| `outcome_title` | Outcome title. |  |
| `outcome_time_frame` | Outcome time frame. |  |
| `measurement_stratum_id` | AACT measurement stratum identifier for raw-derived rows. |  |
| `measurement_title` | Measurement title for raw-derived endpoint rows. |  |
| `measurement_units` | Measurement units for raw-derived endpoint rows. |  |

### BEAR effect fields

| Variable | Definition | Summary |
|:--|:--|:--|
| `measure_class` | Effect-size family used by BEAR summaries. | Standardized Mean Difference 55,371 (33.4%); Probit Difference 33,262 (20.1%); missing 26,616 (16.1%); Mean Difference 19,099 (11.5%); Geometric Ratio 7,637 (4.6%); remaining 23,586 (14.2%) |
| `measure_detailed` | Detailed effect-size label distinguishing Hedges g for raw-derived standardized mean differences from author-reported SMD. |  |
| `scale` | Scale of the stored effect size. | raw 58,412 (35.3%); smd 55,371 (33.4%); probit 33,262 (20.1%); log 18,526 (11.2%) |
| `effect` | Effect estimate used for BEAR-facing analysis. |  |
| `b` | Effect estimate used for z-value calculations where available. |  |
| `se` | Standard error of the effect estimate where available. |  |
| `z` | Signed z-value used in BEAR analyses. |  |
| `z_operator` | Operator describing whether z is exact or bounded. | = 157,158 (94.9%); > 7,512 (4.5%); < 901 (0.5%) |

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
| `non_inferiority_type` | Type of hypothesis tested by the source-reported analysis. |  |
| `non_inferiority_description` | Source-reported free-text description of the non-inferiority or equivalence analysis. |  |
| `non_inferior_desc_exists` | Whether the source-reported analysis has a non_inferiority_description. |  |
| `non_inferior_type_constructed` | Type of hypothesis tested, classified from non_inferiority_description when present and from non_inferiority_type otherwise. |  |
| `non_inferior_margin` | Margin or threshold stated in non_inferiority_description. |  |

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
| `direction_unknown` | Whether treatment-comparator direction could not be inferred for the raw-derived contrast. | TRUE 24,605 (14.9%); missing 76,938 (46.5%) |
| `raw_multi_arm_trial` | Whether the raw-derived contrast comes from a multi-arm trial. | TRUE 50,262 (30.4%); missing 76,938 (46.5%) |
