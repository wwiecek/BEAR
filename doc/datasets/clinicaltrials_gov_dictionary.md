## Data dictionary

The summary column is computed from `clinicaltrialsgov.rds` when this dictionary is regenerated. Counts are row-level effect counts, not unique trial counts.

### Row and source identifiers

| Variable | Definition | Summary |
|:--|:--|:--|
| `effect_id` | Unique row identifier for the merged ClinicalTrials.gov effect row. |  |
| `author_effect_id` | Identifier for the author-reported outcome-analysis effect row. |  |
| `raw_effect_id` | Identifier for the selected raw endpoint calculation row. |  |
| `effect_source` | Source of the effect estimate used for b, se, z, and z_operator. | raw_derived 88,100 (53.6%); author_reported 76,353 (46.4%) |
| `include_in_bear` | Selected by the author-versus-raw rule for possible inclusion in BEAR.rds. When both values are available for the same comparison, BEAR.rds uses the author-reported value; the raw-derived value remains available in this ClinicalTrials.gov dataset with this flag set to FALSE. | TRUE 143,485 (87.2%) |
| `result_id` | BEAR-defined identifier linking calculations for the same trial, outcome and unordered pair of result groups. |  |
| `nct_id` | ClinicalTrials.gov study identifier. |  |
| `outcome_id` | AACT outcome identifier linked to the effect row. |  |
| `outcome_analysis_id` | AACT outcome-analysis identifier for source-reported analyses. |  |

### Study characteristics

| Variable | Definition | Summary |
|:--|:--|:--|
| `brief_title` | Brief trial title. |  |
| `official_title` | Official trial title. |  |
| `study_type` | ClinicalTrials.gov study type. | INTERVENTIONAL 157,755 (95.9%); OBSERVATIONAL 6,698 (4.1%) |
| `phase` | Trial phase as recorded by ClinicalTrials.gov. | missing 48,426 (29.4%); PHASE2 44,300 (26.9%); PHASE3 30,822 (18.7%); PHASE1 20,241 (12.3%); PHASE4 12,148 (7.4%); remaining 8,516 (5.2%) |
| `overall_status` | ClinicalTrials.gov recruitment or completion status. | completed 164,453 (100.0%) |
| `year` | Year used by BEAR for time summaries. | median 2,017 [2,012, 2,021]; range 1,992-2,026 |
| `enrollment` | Trial enrollment count. | median 125 [50, 400]; range 1-4,052,231; missing 2 |
| `enrollment_type` | Whether enrollment is actual or anticipated. |  |
| `number_of_arms` | Number of intervention arms recorded in the ClinicalTrials.gov study design table. | median 2 [2, 4]; range 1-32; missing 7,200 |
| `number_of_groups` | Number of observational groups recorded in the ClinicalTrials.gov study design table; usually missing for interventional trials. | median 1 [1, 2]; range 1-49; missing 158,229 |
| `results_first_posted_year` | Calendar year when results were first posted. |  |
| `primary_completion_year` | Calendar year of primary completion. |  |
| `n_effect_rows_per_study` | Number of public candidate effect rows linked to the same ClinicalTrials.gov study. | median 2 [1, 4]; range 1-2,474 |
| `n_outcomes_per_study` | Number of distinct outcome IDs represented for the same ClinicalTrials.gov study in the public file. | median 1 [1, 2]; range 1-96 |
| `allocation` | Allocation design recorded for the study. The main BEAR ClinicalTrials.gov dataset keeps randomized rows; non-randomized and missing-allocation rows remain identifiable in the public file. | RANDOMIZED 146,260 (88.9%); missing 12,425 (7.6%); NON_RANDOMIZED 5,768 (3.5%) |
| `intervention_model` | Intervention model recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on intervention model, although the raw-derived endpoint path is restricted to eligible randomized parallel interventional trials. | PARALLEL 137,079 (83.4%); CROSSOVER 10,058 (6.1%); SINGLE_GROUP 7,725 (4.7%); missing 6,906 (4.2%); FACTORIAL 1,682 (1.0%); remaining 1,003 (0.6%) |
| `primary_purpose` | Primary purpose recorded for the study. The main BEAR ClinicalTrials.gov dataset does not filter on primary purpose. | TREATMENT 98,313 (59.8%); PREVENTION 31,101 (18.9%); BASIC_SCIENCE 8,541 (5.2%); missing 8,420 (5.1%); OTHER 7,366 (4.5%); remaining 10,712 (6.5%) |
| `observational_model` | Observational model if applicable; this is usually present only for observational studies and is therefore missing for most interventional rows. |  |
| `masking` | Masking design recorded for the study. | NONE 42,051 (25.6%); DOUBLE 38,798 (23.6%); QUADRUPLE 30,771 (18.7%); TRIPLE 27,776 (16.9%); SINGLE 17,918 (10.9%); remaining 7,139 (4.3%) |
| `is_factorial` | Whether the intervention model indicates a factorial design. | TRUE 1,682 (1.0%); missing 18 (0.0%) |
| `is_crossover` | Whether the intervention model indicates a crossover design. | TRUE 10,058 (6.1%); missing 18 (0.0%) |
| `n_design_groups` | Number of design-group records linked to the study. Unlike number_of_arms, this is counted from the design_groups table and is available for comparator summaries. | median 2 [2, 4]; range 0-49 |
| `has_placebo_comparator` | Whether any design-group record appears to be a placebo comparator. | TRUE 63,946 (38.9%) |
| `comparator_summary` | Compact study-level comparator summary derived from design-group labels and descriptions. |  |
| `is_multi_arm` | Whether the study has more than two arms or groups. | TRUE 76,263 (46.4%) |
| `has_baseline_measurements` | Whether baseline measurement rows were available in AACT. | TRUE 164,431 (100.0%) |

### Trial categories

| Variable | Definition | Summary |
|:--|:--|:--|
| `domain_all` | All mapped clinical domains for the trial. |  |
| `domain_n` | Number of mapped clinical domains for the trial. |  |
| `domain_source` | Source used for domain mapping. |  |
| `domain_oncology` | Whether the trial maps to oncology. | TRUE 11,684 (7.1%) |
| `domain_primary` | Primary mapped clinical domain. | infectious_disease 32,130 (19.5%); unknown 22,657 (13.8%); mental_health 18,558 (11.3%); endocrine_metabolic 14,238 (8.7%); neurology 13,247 (8.1%); remaining 63,623 (38.7%) |
| `intervention_types_all` | All intervention types recorded for the study. |  |
| `intervention_type_n` | Number of intervention types recorded for the study. |  |
| `intervention_type_primary` | Primary intervention type used for summaries. | drug 95,813 (58.3%); biological 23,115 (14.1%); behavioral 17,998 (10.9%); device 12,543 (7.6%); other 8,408 (5.1%); remaining 6,576 (4.0%) |
| `is_drug_trial` | Whether any intervention is coded as drug. | TRUE 107,390 (65.3%) |
| `is_biological_trial` | Whether any intervention is coded as biological. | TRUE 31,803 (19.3%) |
| `is_device_trial` | Whether any intervention is coded as device. | TRUE 14,001 (8.5%) |
| `is_procedure_trial` | Whether any intervention is coded as procedure. | TRUE 3,769 (2.3%) |
| `is_behavioral_trial` | Whether any intervention is coded as behavioral. | TRUE 20,705 (12.6%) |
| `is_vaccine_trial` | Whether intervention text indicates a vaccine trial. | TRUE 19,426 (11.8%) |
| `is_cell_or_gene_therapy_trial` | Whether intervention text indicates cell or gene therapy. | TRUE 1,132 (0.7%) |
| `is_diagnostic_trial` | Whether any intervention is coded as diagnostic test. | TRUE 6,499 (4.0%) |
| `lead_sponsor_name` | Lead sponsor name. |  |
| `lead_sponsor_class` | Lead sponsor class. | industry 104,078 (63.3%); other 51,757 (31.5%); other_gov 4,600 (2.8%); nih 3,271 (2.0%); network 747 (0.5%) |
| `has_industry_sponsor` | Whether the lead sponsor class is industry. | TRUE 110,896 (67.4%) |

### Outcome descriptors

| Variable | Definition | Summary |
|:--|:--|:--|
| `outcome_type` | Outcome type recorded in ClinicalTrials.gov. | PRIMARY 164,453 (100.0%) |
| `outcome_title` | Outcome title. |  |
| `outcome_time_frame` | Outcome time frame. |  |
| `measurement_stratum_id` | AACT measurement stratum identifier for raw-derived rows. |  |
| `measurement_title` | Measurement title for raw-derived endpoint rows. |  |
| `measurement_units` | Measurement units for raw-derived endpoint rows. |  |

### BEAR effect fields

| Variable | Definition | Summary |
|:--|:--|:--|
| `measure_class` | Effect-size family used by BEAR summaries. | Standardized Mean Difference 54,891 (33.4%); Probit Difference 33,209 (20.2%); missing 26,476 (16.1%); Mean Difference 18,910 (11.5%); Geometric Ratio 7,605 (4.6%); remaining 23,362 (14.2%) |
| `measure_detailed` | Detailed effect-size label distinguishing Hedges g for raw-derived standardized mean differences from author-reported SMD. |  |
| `scale` | Scale of the stored effect size. | raw 57,936 (35.2%); smd 54,891 (33.4%); probit 33,209 (20.2%); log 18,417 (11.2%) |
| `effect` | Effect estimate used for BEAR-facing analysis. |  |
| `b` | Effect estimate used for z-value calculations where available. |  |
| `se` | Standard error of the effect estimate where available. |  |
| `z` | Signed z-value used in BEAR analyses. |  |
| `z_operator` | Operator describing whether z is exact or bounded. | = 156,071 (94.9%); > 7,485 (4.6%); < 897 (0.5%) |

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
| `direction_unknown` | Whether treatment-comparator direction could not be inferred for the raw-derived contrast. | TRUE 24,416 (14.8%); missing 76,353 (46.4%) |
| `raw_multi_arm_trial` | Whether the raw-derived contrast comes from a multi-arm trial. | TRUE 50,177 (30.5%); missing 76,353 (46.4%) |
