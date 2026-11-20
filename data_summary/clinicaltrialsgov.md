# clinicaltrialsgov

- Rows: 165,571
- Columns: 99

## Variable type: character

|Variable                      | Missing| Unique| Empty|Common                                                        |
|:-----------------------------|-------:|------:|-----:|:-------------------------------------------------------------|
|effect_id                     |       0| 165571|     0|                                                              |
|author_effect_id              |   88633|  76938|     0|                                                              |
|raw_effect_id                 |   76938|  88633|     0|                                                              |
|effect_source                 |       0|      2|     0|raw_derived: 88633; author_reported: 76938                    |
|result_id                     |   36918|  59854|     0|                                                              |
|nct_id                        |       0|  30084|     0|                                                              |
|outcome_id                    |       0|  57182|     0|                                                              |
|outcome_analysis_id           |   88633|  76938|     0|                                                              |
|brief_title                   |       0|  29959|     0|                                                              |
|official_title                |     757|  29558|     0|                                                              |
|study_type                    |       0|      2|     0|INTERVENTIONAL: 158853; OBSERVATIONAL: 6718                   |
|phase                         |   49222|      7|     0|PHASE2: 44408; PHASE3: 30925; PHASE1: 20280                   |
|overall_status                |       0|      1|     0|completed: 165571                                             |
|enrollment_type               |       2|      1|     0|actual: 165569                                                |
|allocation                    |   12505|      2|     0|RANDOMIZED: 147271; NON_RANDOMIZED: 5795                      |
|intervention_model            |    6926|      5|     0|PARALLEL: 138046; CROSSOVER: 10104; SINGLE_GROUP: 7785        |
|primary_purpose               |    8440|      9|     0|TREATMENT: 98923; PREVENTION: 31281; BASIC_SCIENCE: 8565      |
|observational_model           |  158950|      7|     0|COHORT: 4004; CASE_CONTROL: 1099; CASE_ONLY: 1092             |
|masking                       |    7159|      5|     0|NONE: 42545; DOUBLE: 39009; QUADRUPLE: 30911                  |
|comparator_summary            |       0|     16|     0|                                                              |
|domain_all                    |       0|    803|     0|                                                              |
|domain_source                 |       0|      3|     0|mesh: 131689; missing: 22906; text_fallback_only: 10976       |
|domain_primary                |       0|     18|     0|                                                              |
|intervention_types_all        |       0|    185|     0|                                                              |
|intervention_type_primary     |       0|     12|     0|                                                              |
|lead_sponsor_name             |       0|   4605|     0|                                                              |
|lead_sponsor_class            |       0|      5|     0|industry: 104377; other: 52511; other_gov: 4656               |
|outcome_type                  |       0|      1|     0|PRIMARY: 165571                                               |
|outcome_title                 |       0|  50430|     0|                                                              |
|outcome_time_frame            |       0|  25105|     0|                                                              |
|measurement_stratum_id        |   76938|  55520|     0|                                                              |
|measurement_title             |   76938|  24554|     0|                                                              |
|measurement_units             |   76938|   4671|     0|                                                              |
|measure_class                 |   26616|     14|     0|                                                              |
|measure_detailed              |   26616|     14|     0|                                                              |
|scale                         |       0|      4|     0|raw: 58412; smd: 55371; probit: 33262                         |
|z_operator                    |       0|      3|     0|=: 157158; &gt;: 7512; &lt;: 901                              |
|linked_result_group_ids       |  115640|  38866|     0|                                                              |
|linked_group_codes            |  115640|    169|     0|                                                              |
|method                        |  122003|   1973|     0|                                                              |
|param_type                    |  115249|   3486|     0|                                                              |
|p_value_modifier              |  147920|      9|     0|&lt;: 15193; =: 1419; &gt;: 1011                              |
|non_inferiority_type          |   88633|      8|     0|SUPERIORITY: 26260; SUPERIORITY_OR_OTHER: 21353; OTHER: 15276 |
|non_inferiority_description   |  147340|   8259|     0|                                                              |
|non_inferior_type_constructed |   88633|      5|     0|SUPERIORITY: 67837; NON_INFERIORITY: 4669; EQUIVALENCE: 2857  |
|non_inferior_margin           |  158586|    863|     0|                                                              |
|raw_group_t_id                |   76938|  36467|     0|                                                              |
|raw_group_c_id                |   76938|  30836|     0|                                                              |
|raw_group_t_title             |   76938|  16225|     0|                                                              |
|raw_group_c_title             |   76938|  10030|     0|                                                              |

## Variable type: logical

|Variable                      | Missing|   TRUE|  FALSE|
|:-----------------------------|-------:|------:|------:|
|include_in_bear               |       0| 144436|  21135|
|is_factorial                  |      18|   1702| 163851|
|is_crossover                  |      18|  10104| 155449|
|has_placebo_comparator        |       0|  64144| 101427|
|is_multi_arm                  |       0|  76478|  89093|
|has_baseline_measurements     |       0| 165549|     22|
|domain_oncology               |       0|  11775| 153796|
|is_drug_trial                 |       0| 107722|  57849|
|is_biological_trial           |       0|  31844| 133727|
|is_device_trial               |       0|  14233| 151338|
|is_procedure_trial            |       0|   3836| 161735|
|is_behavioral_trial           |       0|  21108| 144463|
|is_vaccine_trial              |       0|  19441| 146130|
|is_cell_or_gene_therapy_trial |       0|   1134| 164437|
|is_diagnostic_trial           |       0|   6569| 159002|
|has_industry_sponsor          |       0| 111219|  54352|
|non_inferior_desc_exists      |   88633|  18231|  58707|
|direction_unknown             |   76938|  24605|  64028|
|raw_multi_arm_trial           |   76938|  50262|  38371|

## Variable type: numeric

|Variable                  | Missing|   Mean|      SD|      Min|      Max| -Inf| +Inf|Hist  |
|:-------------------------|-------:|------:|-------:|--------:|--------:|----:|----:|:-----|
|year                      |       0|   2016|   5.287|     1992|     2026|    0|    0|▁▁▅▇▆ |
|enrollment                |       2|   7463|  120400|        1|  4052000|    0|    0|▇▁▁▁▁ |
|number_of_arms            |    7220|  3.444|   2.673|        1|       32|    0|    0|▇▁▁▁▁ |
|number_of_groups          |  159328|  2.754|   6.762|        1|       49|    0|    0|▇▁▁▁▁ |
|results_first_posted_year |       0|   2019|   4.504|     2008|     2026|    0|    0|▂▅▆▇▇ |
|primary_completion_year   |     149|   2016|   5.267|     1991|     2026|    0|    0|▁▁▅▇▆ |
|n_effect_rows_per_study   |       0|    150|   388.8|        1|     2474|    0|    0|▇▁▁▁▁ |
|n_outcomes_per_study      |       0|  7.786|    11.6|        1|       96|    0|    0|▇▁▁▁▁ |
|n_design_groups           |       0|  3.397|    2.94|        0|       49|    0|    0|▇▁▁▁▁ |
|domain_n                  |       0|  1.534|   1.095|        0|       13|    0|    0|▇▂▁▁▁ |
|intervention_type_n       |       0|  1.811|  0.6809|        0|        7|    0|    0|▅▇▂▁▁ |
|effect                    |   27086|  11.94|    1642|  -108300|   236000|    0|    0|▁▇▁▁▁ |
|b                         |   27086|  11.94|    1642|  -108300|   236000|    0|    0|▁▇▁▁▁ |
|se                        |   27142|  10.01|   560.3|        0|    79440|    0|    0|▇▁▁▁▁ |
|z                         |       0|  2.697|   385.8|   -221.5|   156800|   12|  136|▇▁▁▁▁ |
|n_linked_groups           |  115640|  1.978|  0.7573|        1|       39|    0|    0|▇▁▁▁▁ |
|estimate                  |  115272|   4798| 1066000|  -108300| 2.39e+08|    0|    0|▇▁▁▁▁ |
|lower                     |  120029|  6.658|    2880|  -235700|   194700|    0|    0|▁▁▇▁▁ |
|upper                     |  120048|  95.77|    3358|    -2716|   305800|    0|    0|▇▁▁▁▁ |
|p_value                   |  105812| 0.2428|  0.3199|   -5.516|     18.5|    0|    0|▁▇▁▁▁ |
|ci_percent                |  116045|  93.88|   6.424|   -42.88|      985|    0|    0|▇▁▁▁▁ |
|ci_n_sides                |  116115|   1.99| 0.09723|        1|        2|    0|    0|▁▁▁▁▇ |
|raw_event_t               |  132309|  94.16|   861.1|   0.0234|   144600|    0|    0|▇▁▁▁▁ |
|raw_event_c               |  132309|  84.18|   882.9|    8e-04|   136500|    0|    0|▇▁▁▁▁ |
|raw_n_t                   |   76938|  158.7|    1331|        1|   183000|    0|    0|▇▁▁▁▁ |
|raw_n_c                   |   76938|  140.5|    1326|        2|   205800|    0|    0|▇▁▁▁▁ |
|raw_mean_t                |  110200|   2825|  279800| -1981000| 62100000|    0|    0|▇▁▁▁▁ |
|raw_mean_c                |  110200|   2469|   2e+05|   -49190|    4e+07|    0|    0|▇▁▁▁▁ |
|raw_sd_t                  |  110200|   2672|  353300|  9.6e-08| 79990000|    0|    0|▇▁▁▁▁ |
|raw_sd_c                  |  110200|   2501|  348200|  1.2e-07|    8e+07|    0|    0|▇▁▁▁▁ |

