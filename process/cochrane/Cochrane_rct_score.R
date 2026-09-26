# Classify reviews whose abstract eligibility section indicates RCT-only data.

classify_design_v2 <- function(abstracts) {
  tibble::tibble(id = seq_along(abstracts), abstract = abstracts) %>%
    dplyr::mutate(
      text = stringr::str_to_lower(abstract),
      loc_sel = stringr::str_locate(text, "selection criteria"),
      loc_elig = stringr::str_locate(text, "eligibility criteria"),
      sel_start = loc_sel[, "start"],
      elig_start = loc_elig[, "start"],
      section_start = dplyr::coalesce(sel_start, elig_start),
      loc_dc = stringr::str_locate(text, "data collection"),
      dc_start = loc_dc[, "start"],
      has_selection_section = !is.na(section_start),
      sel_block = dplyr::if_else(
        has_selection_section,
        stringr::str_sub(
          text,
          section_start,
          dplyr::if_else(
            !is.na(dc_start) & dc_start > section_start,
            dc_start - 1L,
            nchar(text)
          )
        ),
        NA_character_
      ),
      # Prefer the eligibility section; use the full abstract as a fallback.
      base_text = dplyr::if_else(!is.na(sel_block), sel_block, text),
      has_rct_word = stringr::str_detect(
        base_text, "\\brandomi[sz]ed\\b"
      ),
      has_rct_abbr = stringr::str_detect(base_text, "\\brct(s)?\\b"),
      has_cluster = stringr::str_detect(
        base_text, "cluster[-‐– ]randomi[sz]ed"
      ),
      has_quasi = stringr::str_detect(
        base_text, "quasi[-‐– ]?(randomi[sz]ed|rct(s)?)"
      ),
      has_nonrandom = stringr::str_detect(
        base_text, "non[-‐– ]?randomi[sz]ed"
      ),
      has_observ = stringr::str_detect(
        base_text, "observational stud(y|ies)"
      ),
      has_cohort = stringr::str_detect(base_text, "cohort stud(y|ies)"),
      has_casectrl = stringr::str_detect(
        base_text, "case[- ]control stud(y|ies)"
      ),
      has_crosssec = stringr::str_detect(
        base_text, "cross[- ]sectional stud(y|ies)"
      ),
      has_timeseries = stringr::str_detect(
        base_text,
        "interrupted time series|before[- ]and[- ]after|pre[- ]post"
      ),
      has_uncontrolled = stringr::str_detect(
        base_text, "uncontrolled stud(y|ies)|single[- ]arm stud(y|ies)"
      ),
      any_rct = has_rct_word | has_rct_abbr | has_cluster,
      any_obs = has_observ | has_cohort | has_casectrl | has_crosssec |
        has_timeseries | has_uncontrolled | has_nonrandom | has_quasi,
      design_class = dplyr::case_when(
        any_rct & !any_obs ~ "RCT_only_candidate",
        !any_rct & any_obs ~ "observational_only_candidate",
        any_rct & any_obs ~ "mixed_or_unclear",
        TRUE ~ "no_design_info"
      ),
      rct_only_high_conf = design_class == "RCT_only_candidate" &
        has_selection_section,
      obs_only_high_conf =
        design_class == "observational_only_candidate" &
        has_selection_section
    ) %>%
    dplyr::select(
      id, design_class, has_selection_section,
      rct_only_high_conf, obs_only_high_conf, dplyr::everything()
    )
}
