# ClinicalTrials.gov / AACT trial-characteristics extraction spec

## Purpose

Create a trial-level characteristics table that can later be joined to both:

1. raw-derived ClinicalTrials.gov endpoint effects, for example `raw_effects_primary_v2.rds` or later versions; and
2. the existing BEAR ClinicalTrials.gov author-reported result table, for example `data/clinicaltrialsgov.rds`.

This spec does **not** merge characteristics into either results table. It creates reusable trial-level metadata keyed by `nct_id`, plus a data dictionary. The merge happens in a later integration step.

## Design principle

Use AACT structured fields whenever possible. Use text classification only where the structured fields do not provide enough information.

The main output should be one row per trial. Multi-valued fields, such as disease domains and intervention types, should be handled by:

- a primary categorical variable in the wide trial-characteristics table; and
- a long support table that preserves all matched categories.

Do not create a large number of permanent `domain_*` dummy variables in the main table. The long table can later be widened if needed.

## Input files

Read the following AACT flat files from the same snapshot used for the endpoint extraction:

- `studies.txt`
- `designs.txt`
- `design_groups.txt`
- `sponsors.txt`
- `browse_conditions.txt`
- `conditions.txt`
- `keywords.txt`, if available
- `interventions.txt`
- `browse_interventions.txt`
- `baseline_measurements.txt`, only to derive `has_baseline_measurements`

Optional but useful for documentation and validation:

- `mesh_terms.txt`
- `mesh_headings.txt`

## Outputs

Write these files under:

`data_raw/clinicaltrials.gov/derived/legacy_analysis_output/`

### 1. `trial_characteristics.rds`

One row per `nct_id`.

Required key:

| variable | type | source | notes |
|---|---|---|---|
| `nct_id` | character | all tables | Primary join key. Must be unique in this table. |

### 2. `trial_domains_long.rds`

One row per `nct_id + domain_source + domain_value`.

Use this to preserve multi-label domain information.

### 3. `trial_interventions_long.rds`

One row per `nct_id + intervention_type_or_flag + source_term`.

Use this to preserve multi-label intervention information.

### 4. `trial_characteristics_dictionary.csv`

Machine-readable data dictionary for all variables created in this spec.

Required columns:

```text
variable
output_table
type
allowed_values
source_table
source_column
cleaning_rule
missing_rule
description
notes
```

The later merge step must extend the dictionary to cover all pre-existing result variables as well. This spec only creates the characteristic-variable part of the dictionary.

### 5. `trial_characteristics_qc_summary.csv`

QC counts and sanity checks.

## Main wide table: `trial_characteristics.rds`

### A. Basic trial fields

Extract from `studies.txt`.

| output variable | source column | cleaning |
|---|---|---|
| `brief_title` | `brief_title` | Preserve raw text. |
| `official_title` | `official_title` | Preserve raw text. |
| `study_type` | `study_type` | Normalize to lower snake case. |
| `phase` | `phase` | Normalize common values, keep original if not recognized. |
| `overall_status` | `overall_status` | Normalize to lower snake case. |
| `enrollment` | `enrollment` | Numeric. |
| `enrollment_type` | `enrollment_type` | Normalize to `actual`, `anticipated`, or `unknown`. |
| `number_of_arms` | `number_of_arms` | Numeric. |
| `number_of_groups` | `number_of_groups` | Numeric. |
| `results_first_posted_date` | `results_first_posted_date` | Date. |
| `primary_completion_date` | `primary_completion_date` | Date if available. |
| `completion_date` | `completion_date` | Date if available. |

Add derived variables:

| output variable | type | rule |
|---|---|---|
| `results_first_posted_year` | integer | Year from `results_first_posted_date`. |
| `primary_completion_year` | integer | Year from `primary_completion_date`. |
| `has_posted_results` | logical | `!is.na(results_first_posted_date)`. |

### B. Design fields

Extract from `designs.txt`.

| output variable | source column | cleaning |
|---|---|---|
| `allocation` | `allocation` | Normalize to lower snake case. |
| `intervention_model` | `intervention_model` | Normalize to lower snake case. |
| `primary_purpose` | `primary_purpose` | Normalize to lower snake case. |
| `masking` | `masking` | Preserve normalized text if available. |

Add derived design flags:

| output variable | type | rule |
|---|---|---|
| `is_multi_arm` | logical | `number_of_arms > 2` or `number_of_groups > 2`; if both missing, use `design_groups` count > 2. |
| `is_factorial` | logical | `intervention_model` contains `factorial`; fallback to title/design text search only if structured value missing. |
| `is_crossover` | logical | `intervention_model` contains `crossover`. |

### C. Comparator design flags

Extract from `design_groups.txt`.

Use `group_type`, `title`, and `description` if available.

Create:

| output variable | type | rule |
|---|---|---|
| `has_placebo_comparator` | logical | TRUE if any design group has `group_type` or title/description indicating placebo. |
| `has_sham_comparator` | logical | TRUE if any design group has `group_type` or title/description indicating sham. |
| `has_no_intervention_comparator` | logical | TRUE if any design group has `group_type` or title/description indicating no intervention, usual care, waitlist, standard care, or control. |
| `has_active_comparator` | logical | TRUE if any design group has `group_type` or title/description indicating active comparator. |
| `comparator_summary` | character | Semicolon-separated set among `placebo`, `sham`, `no_intervention_or_usual_care`, `active`, `none_detected`, `unknown`. |

Structured `group_type` evidence should take priority over keyword evidence.

Keyword fallback examples:

- placebo: `placebo`, `vehicle`
- sham: `sham`
- no intervention / usual care: `no intervention`, `usual care`, `standard care`, `standard of care`, `waitlist`, `control`
- active comparator: `active comparator`, `active control`, `comparator`

Do not use outcome result-group labels here. This is a trial-level design table, not an effect-level orientation table. Effect-level orientation remains part of the endpoint extraction.

### D. Therapeutic area / clinical domain

Use MeSH-based classification as the primary source.

AACT provides `browse_conditions.txt`, which contains NLM-generated MeSH terms for trial conditions. Use these fields if present:

- `nct_id`
- `mesh_term`
- `downcase_mesh_term`, if available
- `mesh_type`, if available

If `mesh_type` is available, prefer ancestor terms because they already encode broader MeSH hierarchy information.

#### Domain output variables in `trial_characteristics.rds`

| output variable | type | rule |
|---|---|---|
| `domain_primary` | categorical character | Single best domain, using the priority rules below. |
| `domain_all` | character | Semicolon-separated domain categories assigned to the trial. |
| `domain_n` | integer | Number of assigned domain categories. |
| `domain_source` | character | `mesh`, `mesh_plus_text_fallback`, `text_fallback_only`, or `missing`. |
| `domain_oncology` | logical | Convenience flag for oncology, because it will often be used directly. |

Although `domain_oncology` is a dummy variable, keep it because oncology is a high-priority domain. Do not create broad dummy variables for every domain in the main table unless needed later.

#### Domain categories

Use a compact BEAR domain grouping derived from standard MeSH condition hierarchy. Start with these categories:

```text
oncology
cardiovascular
infectious_disease
mental_health
neurology
endocrine_metabolic
respiratory
musculoskeletal
gastrointestinal
urology_nephrology
obstetrics_gynecology
hematology_immunology
pediatrics_neonatal_congenital
dermatology
ophthalmology
ear_nose_throat
injury_occupational_environmental
other_or_mixed
unknown
```

Implementation should use a versioned mapping file:

`process/clinicaltrials.gov/spec/domain_mesh_map.csv`

Required columns:

```text
mesh_term_pattern
mesh_tree_prefix_or_category
domain
priority
notes
```

Suggested starting map:

| domain | MeSH anchor terms or disease-tree basis |
|---|---|
| `oncology` | `Neoplasms`, MeSH disease tree C04. |
| `cardiovascular` | `Cardiovascular Diseases`, C14. |
| `infectious_disease` | `Infections`, `Virus Diseases`, `Parasitic Diseases`, C01-C03. |
| `musculoskeletal` | `Musculoskeletal Diseases`, C05. |
| `gastrointestinal` | `Digestive System Diseases`, C06. |
| `respiratory` | `Respiratory Tract Diseases`, C08. |
| `ear_nose_throat` | `Otorhinolaryngologic Diseases`, C09. |
| `neurology` | `Nervous System Diseases`, C10; consider also neurological condition text. |
| `ophthalmology` | `Eye Diseases`, C11. |
| `urology_nephrology` | `Male Urogenital Diseases`, C12, plus kidney/urinary terms. |
| `obstetrics_gynecology` | `Female Urogenital Diseases and Pregnancy Complications`, C13. |
| `hematology_immunology` | `Hemic and Lymphatic Diseases`, C15; `Immune System Diseases`, C20. |
| `pediatrics_neonatal_congenital` | `Congenital, Hereditary, and Neonatal Diseases and Abnormalities`, C16. |
| `dermatology` | `Skin and Connective Tissue Diseases`, C17. |
| `endocrine_metabolic` | `Nutritional and Metabolic Diseases`, C18; `Endocrine System Diseases`, C19. |
| `injury_occupational_environmental` | `Disorders of Environmental Origin`, `Occupational Diseases`, `Chemically-Induced Disorders`, `Wounds and Injuries`, C21, C24-C26. |

Mental-health trials are not perfectly captured by MeSH disease-tree C branches alone. Add a text/MeSH-term rule for `Mental Disorders`, psychiatric, depression, anxiety, psychosis, bipolar, schizophrenia, PTSD, substance use, addiction, and related MeSH ancestors when available. Mark such assignments as `mesh_plus_text_fallback` or `text_fallback_only` depending on source.

#### Primary-domain rule

Many trials have multiple domains. Assign `domain_primary` using this priority:

1. If exactly one domain is assigned, use it.
2. If oncology is present, use `oncology`.
3. If a disease-domain MeSH ancestor appears in `browse_conditions` and maps to one domain, use the highest-priority MeSH-derived domain.
4. If several domains remain, use the lowest numeric `priority` in `domain_mesh_map.csv`.
5. If only text fallback is available, use the highest-priority text-derived domain and set `domain_source = text_fallback_only`.
6. If no domain is assigned, use `unknown`.

Preserve all assigned domains in `trial_domains_long.rds` and `domain_all`.

### E. Intervention class

Use `interventions.txt` as the primary source and `browse_interventions.txt` as a supplement.

#### Output variables in `trial_characteristics.rds`

| output variable | type | rule |
|---|---|---|
| `intervention_type_primary` | categorical character | Single dominant intervention type. |
| `intervention_types_all` | character | Semicolon-separated normalized intervention types. |
| `intervention_type_n` | integer | Number of distinct normalized intervention types. |
| `is_drug_trial` | logical | TRUE if any intervention type is drug or drug-like. |
| `is_biological_trial` | logical | TRUE if any intervention type is biological or biologic-like. |
| `is_device_trial` | logical | TRUE if any intervention type is device. |
| `is_procedure_trial` | logical | TRUE if any intervention type is procedure or surgery. |
| `is_behavioral_trial` | logical | TRUE if any intervention type is behavioral. |
| `is_vaccine_trial` | logical | TRUE if intervention names, MeSH intervention terms, or intervention descriptions indicate vaccine. |
| `is_cell_or_gene_therapy_trial` | logical | TRUE if intervention names, terms, or descriptions indicate cell therapy, gene therapy, CAR-T, stem-cell therapy, viral vector therapy, or related terms. |
| `is_diagnostic_trial` | logical | TRUE if intervention type, name, title, or purpose indicates diagnostic test/imaging/screening tool. |

Normalize AACT intervention types to lower snake case. Preserve all original types in `trial_interventions_long.rds`.

#### Primary intervention-type rule

If multiple intervention types are present, assign `intervention_type_primary` using this priority:

1. drug
2. biological
3. device
4. procedure
5. radiation
6. behavioral
7. diagnostic_test
8. dietary_supplement
9. genetic
10. other
11. unknown

This priority is only for a single categorical summary. The multi-label truth is stored in `intervention_types_all` and `trial_interventions_long.rds`.

### F. Sponsor variables

Extract from `sponsors.txt`.

Keep only:

| output variable | type | rule |
|---|---|---|
| `lead_sponsor_name` | character | Sponsor with `lead_or_collaborator == lead`. Preserve original capitalization. |
| `lead_sponsor_class` | categorical character | Normalized `agency_class` for lead sponsor. |
| `has_industry_sponsor` | logical | TRUE if any sponsor or collaborator has `agency_class` equal to industry. |

Normalize `lead_sponsor_class` to:

```text
industry
nih
us_fed
other_gov
network
individual
other
unknown
```

If AACT values differ, preserve original value in a dictionary note and map to the nearest category.

### G. Baseline-measurement availability

Extract from `baseline_measurements.txt`.

Add:

| output variable | type | rule |
|---|---|---|
| `has_baseline_measurements` | logical | TRUE if the trial has at least one row in `baseline_measurements.txt`. |
| `n_baseline_measurement_rows` | integer | Number of baseline-measurement rows for the trial. |

Do not derive baseline covariate summaries in this spec. This is only an availability flag for later work.

## Long tables

### `trial_domains_long.rds`

Required columns:

```text
nct_id
domain
domain_source
source_table
source_term
mesh_type
matched_rule_id
priority
```

One trial can appear multiple times. This table is the authoritative multi-label domain record.

### `trial_interventions_long.rds`

Required columns:

```text
nct_id
intervention_type_norm
intervention_name
intervention_source
source_table
is_drug_trial
is_biological_trial
is_device_trial
is_procedure_trial
is_behavioral_trial
is_vaccine_trial
is_cell_or_gene_therapy_trial
is_diagnostic_trial
matched_rule_id
```

One trial can appear multiple times.

## Cleaning rules

Use these helper functions or equivalent:

```r
norm_chr <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower()
}

snake <- function(x) {
  x |>
    norm_chr() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}
```

General rules:

- Preserve original source columns in reduced audit files where possible.
- Store normalized fields in the output table.
- Use `NA`, not empty strings, for missing values.
- Use logical `TRUE/FALSE/NA` for flags. Use `NA` when the source table is missing or the derivation could not be evaluated.
- Do not silently collapse conflicting values. Keep a count column such as `domain_n` or `intervention_type_n`.

## QC checks

Create `trial_characteristics_qc_summary.csv` with at least:

1. Number of rows in `trial_characteristics.rds`.
2. Number of distinct `nct_id` values.
3. Assert one row per `nct_id`.
4. Missingness by output variable.
5. Counts for `domain_primary`.
6. Counts for `intervention_type_primary`.
7. Counts for `lead_sponsor_class`.
8. Counts for `primary_purpose` and `intervention_model`.
9. Counts for comparator flags.
10. Number and percent with `has_baseline_measurements == TRUE`.
11. Number and percent where `domain_primary == unknown`.
12. Number and percent where `intervention_type_primary == unknown`.
13. Number of trials with multiple domains.
14. Number of trials with multiple intervention types.

Manual review samples to save:

- `manual_review_domain_unknown.csv`: sample of trials with `domain_primary == unknown`.
- `manual_review_domain_multilabel.csv`: sample of trials with `domain_n > 1`.
- `manual_review_intervention_multitype.csv`: sample of trials with `intervention_type_n > 1`.
- `manual_review_comparator_flags.csv`: sample of trials with comparator flags inferred only from text.

## Data dictionary requirements

Every output variable must have a dictionary row.

For derived variables, `cleaning_rule` should name the function or rule family, for example:

- `normalize_design_value_v1`
- `derive_domain_from_mesh_v1`
- `derive_intervention_flags_v1`
- `derive_comparator_flags_from_design_groups_v1`
- `derive_has_baseline_measurements_v1`

The dictionary should be versioned. If classification rules change later, increment the rule version and update the dictionary.

The dictionary should eventually cover both:

1. this new trial-characteristics table; and
2. pre-existing result/effect variables from the raw-derived and author-reported result tables.

That broader dictionary update is out of scope for this extraction task, but the file format should support it.

## Join plan for later integration

Do not merge in this task.

The next integration step will left-join `trial_characteristics.rds` by `nct_id` to:

- raw-derived endpoint results, for example `raw_effects_primary_v2.rds`; and
- author-reported BEAR ClinicalTrials.gov results, for example `data/clinicaltrialsgov.rds`.

The trial-characteristics table must therefore be stable, one row per `nct_id`, and independent of any particular result-row unit.

Long domain/intervention tables should not be joined directly to effect rows unless the analysis explicitly allows one-to-many expansion. For most analyses, use `domain_primary`, `domain_all`, `intervention_type_primary`, and `intervention_types_all` from the wide table.

## Deliverables

The programmer should deliver:

```text
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/trial_characteristics.rds
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/trial_domains_long.rds
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/trial_interventions_long.rds
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/trial_characteristics_dictionary.csv
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/trial_characteristics_qc_summary.csv
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/manual_review_domain_unknown.csv
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/manual_review_domain_multilabel.csv
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/manual_review_intervention_multitype.csv
data_raw/clinicaltrials.gov/derived/legacy_analysis_output/manual_review_comparator_flags.csv
```

No existing BEAR data files should be modified in this task.
