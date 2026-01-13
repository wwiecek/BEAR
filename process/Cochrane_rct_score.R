# WW asked ChatGPT to compile this code in Dec 2025 to classify reviews that are most likely
# to have only included RCTs

library(dplyr)
library(stringr)

classify_design_v2 <- function(abstracts) {
  tibble(id = seq_along(abstracts),
         abstract = abstracts) %>%
    mutate(
      text = str_to_lower(abstract),
      
      # locate the "Selection/Eligibility criteria" section and its end
      loc_sel  = str_locate(text, "selection criteria"),
      loc_elig = str_locate(text, "eligibility criteria"),
      sel_start  = loc_sel[, "start"],
      elig_start = loc_elig[, "start"],
      section_start = coalesce(sel_start, elig_start),
      
      loc_dc   = str_locate(text, "data collection"),
      dc_start = loc_dc[, "start"],
      
      has_selection_section = !is.na(section_start),
      
      sel_block = if_else(
        has_selection_section,
        str_sub(
          text,
          section_start,
          if_else(!is.na(dc_start) & dc_start > section_start,
                  dc_start - 1L,
                  nchar(text))
        ),
        NA_character_
      ),
      
      # use Selection/Eligibility block if present, otherwise whole abstract
      base_text = if_else(!is.na(sel_block), sel_block, text),
      
      # RCT-ish terms
      has_rct_word = str_detect(base_text, "\\brandomi[sz]ed\\b"),
      has_rct_abbr = str_detect(base_text, "\\brct(s)?\\b"),
      has_cluster  = str_detect(base_text, "cluster[-‐– ]randomi[sz]ed"),
      
      # quasi / non-randomised (treated as "non-RCT" for RCT-only classification)
      has_quasi     = str_detect(base_text, "quasi[-‐– ]?(randomi[sz]ed|rct(s)?)"),
      has_nonrandom = str_detect(base_text, "non[-‐– ]?randomi[sz]ed"),
      
      # observational / non-RCT designs
      has_observ     = str_detect(base_text, "observational stud(y|ies)"),
      has_cohort     = str_detect(base_text, "cohort stud(y|ies)"),
      has_casectrl   = str_detect(base_text, "case[- ]control stud(y|ies)"),
      has_crosssec   = str_detect(base_text, "cross[- ]sectional stud(y|ies)"),
      has_timeseries = str_detect(
        base_text,
        "interrupted time series|before[- ]and[- ]after|pre[- ]post"
      ),
      has_uncontrolled = str_detect(
        base_text,
        "uncontrolled stud(y|ies)|single[- ]arm stud(y|ies)"
      ),
      
      any_rct = has_rct_word | has_rct_abbr | has_cluster,
      any_obs = has_observ | has_cohort | has_casectrl | has_crosssec |
        has_timeseries | has_uncontrolled | has_nonrandom | has_quasi,
      
      design_class = case_when(
        any_rct & !any_obs ~ "RCT_only_candidate",
        !any_rct & any_obs ~ "observational_only_candidate",
        any_rct & any_obs  ~ "mixed_or_unclear",
        TRUE               ~ "no_design_info"
      ),
      
      rct_only_high_conf =
        design_class == "RCT_only_candidate" & has_selection_section,
      
      obs_only_high_conf =
        design_class == "observational_only_candidate" & has_selection_section
    ) %>%
    select(id, design_class, has_selection_section,
           rct_only_high_conf, obs_only_high_conf, everything())
}
