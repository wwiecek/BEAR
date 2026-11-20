# Rules that standardise non_inferiority_type and extract test margins from
# non_inferiority_description for author-reported ClinicalTrials.gov rows.
# Used by step 07 (rule-based columns); recorded manual review decisions are
# applied later in the pipeline. Rules were developed in
# explore/aact_snapshot/non_inferiority_margins/ (see its changelog).

ctgov_ni_types <- c("SUPERIORITY", "NON_INFERIORITY", "EQUIVALENCE",
                    "NON_INFERIORITY_OR_EQUIVALENCE", "UNKNOWN")

# Mapping from non_inferiority_type, used for rows without a description and
# for descriptions that defer to the row's type
ctgov_ni_map_type <- function(type) {
  case_when(
    type %in% c("SUPERIORITY", "SUPERIORITY_OR_OTHER",
                "SUPERIORITY_OR_OTHER_LEGACY", "OTHER") ~ "SUPERIORITY",
    type == "NON_INFERIORITY" ~ "NON_INFERIORITY",
    type == "EQUIVALENCE" ~ "EQUIVALENCE",
    type %in% c("NON_INFERIORITY_OR_EQUIVALENCE",
                "NON_INFERIORITY_OR_EQUIVALENCE_LEGACY") ~
      "NON_INFERIORITY_OR_EQUIVALENCE",
    TRUE ~ "UNKNOWN"
  )
}

# Row-level type from a description-level type. FROM_TYPE keeps the row's
# type mapping, since one description can be used with several types.
# NO_SIGNAL descriptions say nothing about the test: SUPERIORITY on
# superiority types, UNKNOWN otherwise
ctgov_ni_resolve_type <- function(description_type, non_inferiority_type,
                                  non_inferior_desc_exists) {
  mapped <- ctgov_ni_map_type(non_inferiority_type)
  case_when(
    !non_inferior_desc_exists | description_type == "FROM_TYPE" ~ mapped,
    description_type == "NO_SIGNAL" ~
      if_else(mapped == "SUPERIORITY", "SUPERIORITY", "UNKNOWN"),
    TRUE ~ description_type
  )
}

ctgov_ni_description_id <- function(description) {
  map_chr(description, digest::digest, algo = "md5", serialize = FALSE)
}

# Margin extraction -----

ctgov_ni_num <- "(?:[-−+]\\s?)?(?:\\d+(?:\\.\\d+)?|\\.\\d+)(?:/\\d+(?:\\.\\d+)?)?"
ctgov_ni_unit <- paste0(
  "(?:\\s*(?:%|percent(?:age)?\\s*points?|percent\\b|points?\\b|pp\\b)",
  "|[\\s-]*(?:letters?\\b|ms(?:ec)?\\b|mm\\s*Hg|mm\\b|cm\\b|log10|LogMAR|L/min|mL\\b|",
  "g/L|g/dL|bpm\\b|D\\b|seconds?\\b|days?\\b|months?\\b|SD\\b|standard deviations?\\b|",
  "(?:NRS|MADRS)\\s*units?|units?\\b|fold\\b))?")

ctgov_ni_first_match <- function(text, pattern) {
  out <- str_match(text, regex(pattern, ignore_case = TRUE))
  out[, ncol(out)]
}

ctgov_ni_to_num <- function(x) {
  suppressWarnings(as.numeric(str_remove_all(str_replace_all(x, "−", "-"), "[\\s+]")))
}

# Returns the margin or threshold as stated, as text (e.g. "margin -10%",
# "0.80-1.25 (ratio)", "upper bound < 10%"), or NA
ctgov_ni_extract_margin <- function(text) {
  num <- ctgov_ni_num
  unit <- ctgov_ni_unit
  first_match <- ctgov_ni_first_match
  to_num <- ctgov_ni_to_num

  # Equivalence ranges such as 0.80-1.25 or 80%-125%; the first pair of
  # numbers that forms a ratio range counts ("AUC0-192 ... 80.00% to 125.00%").
  # Ranges on the percent scale need a "%" sign, except the standard 80-125
  ranges <- str_match_all(text, regex(paste0(
    "(", num, ")\\s*(%?)\\s*(?:to|-|–|and|,|;)\\s*(", num, ")\\s*(%?)"),
    ignore_case = TRUE))
  range_text <- map_chr(ranges, function(range) {
    lo <- to_num(range[, 2])
    hi <- to_num(range[, 4])
    is_ratio_range <- !is.na(lo) & !is.na(hi) &
      ((lo >= 0.5 & lo < 1 & hi > 1 & hi <= 2) |
         (lo >= 50 & lo < 100 & hi > 100 & hi <= 200 &
            (range[, 3] == "%" | range[, 5] == "%" | (lo == 80 & hi == 125))))
    if (!any(is_ratio_range)) return(NA_character_)
    i <- which(is_ratio_range)[1]
    paste0(range[i, 2], range[i, 3], "-", range[i, 4], range[i, 5], " (ratio)")
  })

  # Skip numbers that are CI levels or sidedness ("95% CI", "2-sided")
  not_ci <- "(?![\\d.]*\\s*-?\\s*sided)(?![\\d.]*\\s*%\\s*(?:CI|confidence|two|one))"

  # Difference ranges around zero after a margin word, such as
  # "equivalence limits of -1.0 and 1.0" or "range (-1.5 mmHg, 1.5 mmHg)".
  # A comma separates the bounds only inside brackets or before a "+" sign,
  # so "a margin of -15%, 5% type 1 error" is not a range
  srange <- str_match(text, regex(paste0(
    "(?:margins?|limits?|bounds?|boundar(?:y|ies)|range(?!\\s*from)|within)[^0-9−+±-]{0,40}?",
    "([\\[(]?)\\s*(", num, ")(", unit, ")\\s*(to|and|,|;|–)\\s*", not_ci, "(", num, ")(", unit, ")"),
    ignore_case = TRUE))
  slo <- to_num(srange[, 3])
  shi <- to_num(srange[, 6])
  comma_ok <- !srange[, 5] %in% c(",", ";") | srange[, 2] != "" |
    str_detect(coalesce(srange[, 6], ""), "^\\+")
  srange_text <- if_else(!is.na(slo) & !is.na(shi) & slo < 0 & shi > 0 & comma_ok,
                         paste0(srange[, 3], srange[, 4], " to ", srange[, 6], srange[, 7]),
                         NA_character_)
  # Bracketed ranges symmetric around zero anywhere in the text ("[-5%;5%] margin");
  # reported confidence intervals are rarely exactly symmetric
  bracket <- str_match(text, regex(paste0(
    "[\\[(]\\s*(", num, ")(", unit, ")\\s*[,;]\\s*(", num, ")(", unit, ")\\s*[\\])]"),
    ignore_case = TRUE))
  blo <- to_num(bracket[, 2])
  bhi <- to_num(bracket[, 4])
  bracket_text <- if_else(!is.na(blo) & !is.na(bhi) & blo < 0 & abs(blo + bhi) < 1e-9,
                          paste0(bracket[, 2], bracket[, 3], " to ", bracket[, 4], bracket[, 5]),
                          NA_character_)

  # Skip numbers that define the outcome ("≥ 0.1 IU/mL", "≥ 1:40", "<50 copies",
  # "> 39.0°C") or start a range ("within 0-14 days")
  not_unit <- paste0("(?![\\d.]+\\s*[-–]\\s*\\d)(?!(?:[-−+]\\s?)?[\\d.]+(?:/[\\d.]+)?\\s*",
                     "(?:[mµμun]g/m?L|m?IU|EU/|U/mL|copies|c/mL|cells|-?\\s?fold\\s*(?:rise|increase)|",
                     ":\\d|ED\\d+|°|years?))")
  pm <- "(?:\\+/-|±|\\+\\s*or\\s*-|plus\\s*or\\s*minus)?\\s*"
  ni_word <- "(?:non[- ]?inferiority|NI|equivalence|clinical)"

  # Margin stated before the keyword: "a 4-letter margin", "0.4% noninferiority margin"
  before_text <- first_match(text, paste0(
    "(?<![\\w.])(", pm, not_ci, num, unit, ")\\s*(?:of\\s*)?[\\s(-]*",
    "(?:", ni_word, "\\s*)?(?:margin|limit|delta)\\b(?!\\s*of\\s*error)"))
  # Margin stated after the keyword: "non-inferiority margin of -10%",
  # "margin of non-inferiority is 0.5", "Δ = 10 points". Δ and δ count only
  # when defined ("δ = 2%"), not as part of a variable name ("ΔQTcF")
  margin_key <- paste0(
    "(?:margin\\w*\\b(?!\\s*of\\s*error)|",
    "delta\\b|(?:Δ|∆|δ)(?=\\s*(?:\\(\\w+\\)\\s*)?(?:=|is\\b|of\\b|was\\b|:))|",
    "(?:inferiority|equivalence|NI\\)?|clinical)\\s*",
    "(?:limits?|bounds?|boundary|boundaries|critical value|range)|",
    "(?:limits?|bounds?|boundary)\\s*(?:of|for)\\s*(?:the\\s*)?non[- ]?inferiority)")
  # The words between keyword and value may not introduce another quantity;
  # numbers attached to a word ("IGF-1", "HbA1c") are skipped
  margin_gap <- paste0(
    "(?:(?!p-?value|alpha|α|power|significan|confidence|\\bCI\\b|sided|sample|\\bSD\\b|standard deviation)",
    "(?:[^0-9−.±+-]|(?<=[A-Za-z])-(?=[A-Za-z])|(?<=[A-Za-z])-?\\d+(?:\\.\\d+)?)){0,60}")
  margin_text <- first_match(text, paste0(
    margin_key, margin_gap, "(", pm, "(?<![A-Za-z0-9.])", not_ci, num, unit, ")"))

  # Confidence bounds compared with a value. A sentence may contain decimals
  # ("97.5% CI") and abbreviations ("S. pneumoniae"), so bound phrases stop at
  # ";" or at ". " before a capital letter
  gap <- "(?:[^;.]|\\.(?=\\d|\\s*(?-i:[a-z(])|[A-Za-z]\\.)){0,250}?"
  qual <- "(?:[\\s-]+(?:\\d+(?:\\.\\d+)?%?|(?:one|two|1|2)-?\\s?sided|GMR|CI)){0,3}"
  bound_word <- "[\\s-]*(?:confidence\\s*)?(?:bound|limit|boundary|end(?:point)?|CL|CI)s?\\b"
  lower_key <- paste0("(?:\\b(?:lower|lowest)", qual, bound_word, "|\\bLL\\b|\\bLCB\\b)")
  upper_key <- paste0("(?:\\bupper", qual, bound_word, "|\\bUL\\b|\\bUCB\\b)")
  lower_op <- paste0(
    "(?:>=|≥|=>|>\\s*/?\\s*=|>\\s*or\\s*=|>|",
    "(?:greater|higher|larger)(?:\\s*than)?\\s*or\\s*equal(?:\\s*to)?|",
    "greater than|larger than|higher than|above|exceed\\w*|more than|",
    "not\\s*(?:be\\s*)?(?:lower|less|smaller) than|no (?:lower|less) than)")
  upper_op <- paste0(
    "(?:<=|≤|=<|<\\s*/?\\s*=|<\\s*or\\s*=|<|",
    "(?:less|lower|smaller|below)(?:\\s*than)?\\s*or\\s*equal(?:\\s*to)?|",
    "less than|lower than|smaller than|(?:strictly\\s*)?below|",
    "(?:should|did|does|would)?\\s*not\\s*exceed|no (?:more|greater|higher|larger) than|",
    "not\\s*(?:be\\s*)?(?:greater|more|larger|higher) than|within)")
  op_value <- paste0(
    "\\s*(?:\\([<>≤≥=/ ]+\\)\\s*)?(?:to\\s*)?",
    "(?:(?:a|an|the)\\s*)?(?:pre-?(?:defined|specified|determined|stated)\\s*)?(?:a-?priori\\s*)?",
    "(?:(?:clinical|non-?inferiority|noninferiority|equivalence)\\s*(?:\\(NI\\)\\s*)?)?",
    "(?:limits?|margin|criterion|threshold|value|bound|boundary)?\\s*(?:of\\s*)?",
    pm, "[\\[(]?\\s*", not_unit, "(", num, unit, ")")
  # Comparison after the value: "was -12% or greater", "1.15 or less"
  post_value <- paste0("(?<![\\w.])", not_unit, "(", num, unit,
                       ")\\s*(?:\\([^)]{0,10}\\)\\s*)?\\]?\\s*or\\s*")
  bound_patterns <- c(
    "lower bound > " = paste0(lower_key, gap, lower_op, op_value),
    "upper bound < " = paste0(upper_key, gap, upper_op, op_value),
    "lower bound > " = paste0(lower_key, gap, post_value, "(?:greater|higher|more|above|larger)"),
    "upper bound < " = paste0(upper_key, gap, post_value, "(?:less|lower|below|smaller)"))
  # When a text states several bounds, the one stated first counts
  bound_value <- sapply(seq_along(bound_patterns), function(j) {
    value <- first_match(text, bound_patterns[j])
    if_else(is.na(value), NA_character_, paste0(names(bound_patterns)[j], value))
  })
  bound_start <- sapply(bound_patterns, function(pattern) {
    str_locate(text, regex(pattern, ignore_case = TRUE))[, 1]
  })
  bound_value <- matrix(bound_value, nrow = length(text))
  bound_start <- matrix(bound_start, nrow = length(text))
  bound_text <- map_chr(seq_along(text), function(i) {
    if (all(is.na(bound_start[i, ]))) NA_character_ else bound_value[i, which.min(bound_start[i, ])]
  })
  # "within ±1.5 mmHg"
  within_text <- first_match(text, paste0(
    "within\\s*(?:the\\s*)?((?:\\+/-|±|\\+\\s*or\\s*-|plus\\s*or\\s*minus)\\s*", num, unit, ")"))
  goal_text <- first_match(text, paste0(
    "(?:performance goal|objective performance criteri\\w+|comparison value)",
    "[^0-9]{0,30}(", num, unit, ")"))

  coalesce(
    range_text,
    srange_text,
    bracket_text,
    if_else(!is.na(before_text), paste0("margin ", before_text), NA_character_),
    if_else(!is.na(margin_text), paste0("margin ", margin_text), NA_character_),
    bound_text,
    if_else(!is.na(within_text), paste0("margin ", within_text), NA_character_),
    if_else(!is.na(goal_text), paste0("threshold ", goal_text), NA_character_)
  ) %>% str_squish()
}

# Description-level classification -----

# One row per distinct description among `rows` (which needs
# non_inferiority_type and non_inferiority_description). rule_type is a
# standardised type, FROM_TYPE or NO_SIGNAL; review_reason marks descriptions
# the rules cannot settle
ctgov_ni_classify_descriptions <- function(rows) {
  rows %>%
    filter(!is.na(non_inferiority_description),
           str_trim(non_inferiority_description) != "") %>%
    group_by(non_inferiority_description) %>%
    summarise(
      rows = n(),
      aact_types = paste(sort(unique(non_inferiority_type)), collapse = ";"),
      aact_mapped = paste(sort(unique(ctgov_ni_map_type(non_inferiority_type))),
                          collapse = ";"),
      .groups = "drop"
    ) %>%
    mutate(
      description_id = ctgov_ni_description_id(non_inferiority_description),
      # Registry text escapes characters such as "<", ">" and "[" with a backslash
      text = str_replace_all(non_inferiority_description, "\\\\([<>\\[\\]&_*])", "\\1"),
      has_ni = str_detect(text, regex(
        "non[- ]?inferior|noninferior|not (?:be )?inferior", ignore_case = TRUE)),
      # "equivalent(ly)" is common in non-inferiority text, so it is not a signal
      has_eq = str_detect(text, regex(
        "equivalence|equivalency|bioequivalen|two one[- ]sided tests?|\\bTOST\\b",
        ignore_case = TRUE)),
      has_sup = str_detect(text, regex("superior", ignore_case = TRUE)),
      has_goal = str_detect(text, regex(
        "performance goal|objective performance|comparison value|one[- ]sample|single[- ]arm",
        ignore_case = TRUE)),
      # Descriptions without a test keyword: a test against no difference, or a
      # comparison of formulations' pharmacokinetics (usually judged against
      # 80-125%); other pharmacokinetic analyses (interactions, organ
      # impairment) are not a signal
      has_diff = str_detect(text, regex(paste0(
        "p[- ]?values?\\s*(?:of\\s*)?(?:was\\s*)?(?:<|≤|less than|below)|\\bp\\s*(?:<|≤)|",
        "statistical(?:ly)?\\s*significan|significan(?:t|ce)\\s*(?:level|difference|at)|",
        "considered significant|significantly (?:different|greater|less|higher|lower|better|worse)|",
        "no (?:statistically significant |significant |mean )?(?:difference|change)|",
        "(?:power|powered) to detect|detect a difference|is there a difference|not equal|",
        "null hypothes\\w*[^.;]{0,120}?(?:\\bequal|no (?:\\w+ )?(?:difference|effect|change)|identical|\\bzero\\b)|",
        "\\bH0\\s*:?\\s*[^,.;≤≥<>=]{1,20}=|\\bt[- ]tests?\\b|wilcoxon|mann[- ]whitney|chi[- ]?squared?|",
        "fisher'?s exact|log[- ]rank|main effect|interaction effect"), ignore_case = TRUE)),
      has_pk = str_detect(text, regex("AUC|C ?max|pharmacokinetic|bioavailability|\\bPK\\b",
                                      ignore_case = TRUE)) &
        str_detect(text, regex(paste0("bioavailab|formulation|test\\W{0,3}(?:\\w+\\W+){0,3}reference|",
                                      "tablet|capsule|generic|brand"), ignore_case = TRUE)),
      keyword_less = !has_ni & !has_eq & !has_sup,
      rule_margin = ctgov_ni_extract_margin(text),
      rule_type = case_when(
        has_ni & has_eq ~ "NON_INFERIORITY_OR_EQUIVALENCE",
        has_ni ~ "NON_INFERIORITY",
        has_eq ~ "EQUIVALENCE",
        str_detect(coalesce(rule_margin, ""), "\\(ratio\\)| to ") ~ "EQUIVALENCE",
        has_sup ~ "SUPERIORITY",
        # A stated margin agrees with the row's non-inferiority or equivalence type
        !is.na(rule_margin) ~ "FROM_TYPE",
        has_diff ~ "SUPERIORITY",
        has_pk ~ "EQUIVALENCE",
        TRUE ~ "NO_SIGNAL"
      ),
      review_reason = case_when(
        keyword_less & is.na(rule_margin) &
          (str_detect(aact_mapped, "NON_INFERIORITY|EQUIVALENCE") | rule_type == "EQUIVALENCE") ~
          "no test keyword: classified from description",
        has_ni & has_eq ~ "non-inferiority and equivalence both mentioned",
        has_sup & (has_ni | has_eq) ~ "superiority and non-inferiority/equivalence both mentioned",
        has_goal ~ "threshold or single-arm test",
        !rule_type %in% c("FROM_TYPE", "NO_SIGNAL") & !str_detect(aact_mapped, fixed(rule_type)) ~
          "description disagrees with non_inferiority_type",
        !rule_type %in% c("FROM_TYPE", "NO_SIGNAL") & str_detect(aact_mapped, ";") ~
          "description used with several types",
        (has_ni | has_eq) & is.na(rule_margin) &
          str_detect(text, regex("margin|bound|limit", ignore_case = TRUE)) ~
          "margin mentioned but not extracted",
        TRUE ~ NA_character_
      )
    ) %>%
    select(description_id, non_inferiority_description, rows, aact_types,
           rule_type, rule_margin, review_reason)
}
