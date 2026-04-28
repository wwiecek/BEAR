# Helper functions for processing the SCORE replication package into BEAR-ready
# data frames.

suppressPackageStartupMessages(library(tidyverse))

score_number_pattern <- "[-+]?(?:\\d+\\.\\d+|\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?"
score_operator_pattern <- "(?:<=|>=|<|>|=|<=|>=|≤|≥)"

score_parse_number <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

score_normalize_operator <- function(op) {
  case_when(
    is.na(op) ~ NA_character_,
    op %in% c("<", "<=", "≤") ~ "<",
    op %in% c(">", ">=", "≥") ~ ">",
    TRUE ~ "="
  )
}

score_first_match <- function(text, pattern) {
  stringr::str_match(text, stringr::regex(pattern, ignore_case = TRUE))[1, ]
}

z_from_two_sided_p <- function(p) {
  p <- as.numeric(p)
  p <- ifelse(is.na(p), NA_real_, p)
  p <- pmax(pmin(p, 1 - 1e-16), 1e-300)
  qnorm(1 - p / 2)
}

sign_nonzero <- function(x) {
  x <- as.numeric(x)
  out <- sign(x)
  out[out == 0] <- NA_real_
  out
}

truncate_score_z <- function(z, limit = 20) {
  case_when(
    is.na(z) ~ NA_real_,
    !is.finite(z) & z > 0 ~ limit,
    !is.finite(z) & z < 0 ~ -limit,
    z > limit ~ limit,
    z < -limit ~ -limit,
    TRUE ~ z
  )
}

derive_preferred_z <- function(data,
                               coef_value,
                               coef_se,
                               stat_type,
                               stat_value,
                               stat_dof_1,
                               p_value,
                               p_value_type,
                               sign_hint) {
  stat_type_value <- tolower(as.character(data[[stat_type]]))
  coef_value_num <- as.numeric(data[[coef_value]])
  coef_se_num <- as.numeric(data[[coef_se]])
  stat_value_num <- as.numeric(data[[stat_value]])
  stat_dof_1_num <- as.numeric(data[[stat_dof_1]])
  p_value_num <- as.numeric(data[[p_value]])
  p_value_type_chr <- as.character(data[[p_value_type]])
  sign_hint_num <- as.numeric(data[[sign_hint]])
  sqrt_f_stat <- rep(NA_real_, length(stat_value_num))
  valid_f_idx <- !is.na(stat_value_num) & stat_value_num >= 0
  sqrt_f_stat[valid_f_idx] <- sqrt(stat_value_num[valid_f_idx])

  z_from_coef <- ifelse(
    !is.na(coef_value_num) & !is.na(coef_se_num) & coef_se_num > 0,
    coef_value_num / coef_se_num,
    NA_real_
  )

  z_from_stat <- case_when(
    stat_type_value == "z" & !is.na(stat_value_num) ~ stat_value_num,
    stat_type_value == "t" & !is.na(stat_value_num) ~ stat_value_num,
    stat_type_value == "f" &
      !is.na(stat_value_num) &
      !is.na(stat_dof_1_num) &
      stat_dof_1_num == 1 &
      !is.na(sign_hint_num) ~ sign_hint_num * sqrt_f_stat,
    TRUE ~ NA_real_
  )

  z_from_p <- ifelse(
    !is.na(p_value_num) & !is.na(sign_hint_num),
    sign_hint_num * z_from_two_sided_p(p_value_num),
    NA_real_
  )

  z_preferred <- dplyr::coalesce(z_from_coef, z_from_stat, z_from_p)

  z_source <- case_when(
    !is.na(z_from_coef) ~ "coef_over_se",
    !is.na(z_from_stat) & stat_type_value == "f" ~ "sqrt_f_df1_eq_1",
    !is.na(z_from_stat) ~ "reported_stat",
    !is.na(z_from_p) ~ "two_sided_p",
    TRUE ~ NA_character_
  )

  z_operator <- case_when(
    !is.na(z_from_coef) | !is.na(z_from_stat) ~ "=",
    !is.na(z_from_p) &
      p_value_type_chr %in% c("less-than", "less_than", "lt") ~ ">",
    !is.na(z_from_p) & p_value_num == 0 ~ ">",
    !is.na(z_from_p) ~ "=",
    TRUE ~ NA_character_
  )

  data %>%
    mutate(
      z_from_coef = z_from_coef,
      z_from_stat = z_from_stat,
      z_from_p = z_from_p,
      z_preferred = z_preferred,
      z_source = z_source,
      z_operator = z_operator
    )
}

derive_significance <- function(p_value, z_value) {
  p_value_num <- as.numeric(p_value)
  z_value_num <- as.numeric(z_value)

  case_when(
    !is.na(p_value_num) ~ p_value_num <= 0.05,
    !is.na(z_value_num) ~ abs(z_value_num) >= 1.96,
    TRUE ~ NA
  )
}

conv_r_se_from_bounds <- function(b, lb, ub, z) {
  b_num <- as.numeric(b)
  lb_num <- as.numeric(lb)
  ub_num <- as.numeric(ub)
  z_num <- as.numeric(z)

  se_from_ci <- ifelse(
    !is.na(lb_num) & !is.na(ub_num) & ub_num > lb_num,
    (ub_num - lb_num) / (2 * 1.96),
    NA_real_
  )

  se_from_z <- ifelse(
    !is.na(b_num) & !is.na(z_num) & z_num != 0,
    abs(b_num / z_num),
    NA_real_
  )

  dplyr::coalesce(se_from_ci, se_from_z)
}

score_z_sign_from_context <- function(text, b, effect_source) {
  if (!is.na(b) && b != 0) {
    return(list(sign = sign(b), source = effect_source))
  }

  mean_pattern <- paste0(
    "\\bM[^=,;]*=\\s*(", score_number_pattern, ")%?\\s*",
    "(?:vs\\.?|,)\\s*\\bM[^=,;]*=\\s*(",
    score_number_pattern, ")%?"
  )
  mean_match <- score_first_match(text, mean_pattern)
  if (length(mean_match) >= 3 && !is.na(mean_match[2])) {
    diff <- score_parse_number(mean_match[2]) - score_parse_number(mean_match[3])
    if (!is.na(diff) && diff != 0) {
      return(list(sign = sign(diff), source = "mean_contrast"))
    }
  }

  list(sign = 1, source = "unsigned")
}

score_extract_reported_z <- function(text) {
  pattern <- paste0(
    "\\bz\\b\\s*(", score_operator_pattern, ")\\s*(",
    score_number_pattern, ")"
  )
  match <- score_first_match(text, pattern)
  if (length(match) < 3 || is.na(match[2])) {
    return(list(value = NA_real_, operator = NA_character_))
  }

  list(value = score_parse_number(match[3]),
       operator = score_normalize_operator(match[2]))
}

score_extract_reported_t <- function(text) {
  pattern <- paste0(
    "\\bt\\s*(?:\\([^)]*\\))?\\s*(", score_operator_pattern,
    ")\\s*(", score_number_pattern, ")"
  )
  match <- score_first_match(text, pattern)
  if (length(match) < 3 || is.na(match[2])) {
    return(list(value = NA_real_, operator = NA_character_))
  }

  list(value = score_parse_number(match[3]),
       operator = score_normalize_operator(match[2]))
}

score_extract_reported_f <- function(text) {
  pattern <- paste0(
    "\\bf\\s*\\(\\s*(", score_number_pattern, ")\\s*,\\s*(",
    score_number_pattern, ")\\s*\\)\\s*(", score_operator_pattern,
    ")\\s*(", score_number_pattern, ")"
  )
  match <- score_first_match(text, pattern)
  if (length(match) < 5 || is.na(match[4])) {
    return(list(value = NA_real_, df1 = NA_real_, operator = NA_character_))
  }

  list(
    value = score_parse_number(match[5]),
    df1 = score_parse_number(match[2]),
    operator = score_normalize_operator(match[4])
  )
}

score_extract_p <- function(text) {
  pattern <- paste0(
    "\\bp\\s*(", score_operator_pattern, ")\\s*(",
    score_number_pattern, ")"
  )
  match <- score_first_match(text, pattern)
  if (length(match) < 3 || is.na(match[2])) {
    return(list(value = NA_real_, operator = NA_character_))
  }

  list(value = score_parse_number(match[3]),
       operator = score_normalize_operator(match[2]))
}

score_extract_effect <- function(text) {
  effect_pattern <- paste0(
    "(partial\\s+eta[- ]?squared|eta[- ]?squared|beta|β|coefficient|",
    "coef|estimate|effect|\\bb\\b|\\br\\b|\\bd\\b)\\s*",
    "(?:\\[[^]]*\\])?\\s*(?:=|:)\\s*(", score_number_pattern, ")"
  )
  match <- score_first_match(text, effect_pattern)
  if (length(match) >= 3 && !is.na(match[2])) {
    measure <- stringr::str_to_lower(match[2]) %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_replace("eta squared", "eta-squared")
    return(list(
      value = score_parse_number(match[3]),
      measure = measure,
      source = "labelled_effect"
    ))
  }

  unlabelled_pattern <- paste0(
    "(?:^|:)\\s*(", score_number_pattern, ")\\s*,\\s*",
    "(?:standard\\s+error|s\\.?e\\.?|se)\\s*(?:=|:)\\s*",
    score_number_pattern
  )
  unlabelled_match <- score_first_match(text, unlabelled_pattern)
  if (length(unlabelled_match) >= 2 && !is.na(unlabelled_match[2])) {
    return(list(
      value = score_parse_number(unlabelled_match[2]),
      measure = NA_character_,
      source = "nearby_unlabelled_effect"
    ))
  }

  list(value = NA_real_, measure = NA_character_, source = "missing")
}

score_extract_se <- function(text) {
  pattern <- paste0(
    "(?:standard\\s+error|s\\.?e\\.?|\\bse\\b)\\s*(?:=|:)\\s*(",
    score_number_pattern, ")"
  )
  match <- score_first_match(text, pattern)
  if (length(match) < 2 || is.na(match[2])) {
    return(list(value = NA_real_, source = "missing"))
  }

  list(value = score_parse_number(match[2]), source = "labelled_se")
}

score_extract_ci <- function(text) {
  patterns <- c(
    paste0(
      "95\\s*%?\\s*(?:confidence\\s+interval|ci).*?ll\\s*=\\s*(",
      score_number_pattern, ")\\s*,\\s*ul\\s*=\\s*(",
      score_number_pattern, ")"
    ),
    paste0(
      "95\\s*%?\\s*(?:confidence\\s+interval|ci)[^\\[]*\\[\\s*(",
      score_number_pattern, ")\\s*,\\s*(", score_number_pattern,
      ")\\s*\\]"
    ),
    paste0(
      "95\\s*%?\\s*(?:confidence\\s+interval|ci).*?(?:=|:)\\s*(",
      score_number_pattern, ")\\s*(?:to|,)\\s*(",
      score_number_pattern, ")"
    )
  )

  for (pattern in patterns) {
    match <- score_first_match(text, pattern)
    if (length(match) >= 3 && !is.na(match[2])) {
      lower <- score_parse_number(match[2])
      upper <- score_parse_number(match[3])
      if (!is.na(lower) && !is.na(upper) && upper > lower) {
        return(list(lower = lower, upper = upper,
                    se = (upper - lower) / 3.92))
      }
    }
  }

  list(lower = NA_real_, upper = NA_real_, se = NA_real_)
}

parse_score_fragment <- function(text) {
  z_reported <- score_extract_reported_z(text)
  t_reported <- score_extract_reported_t(text)
  f_reported <- score_extract_reported_f(text)
  p_reported <- score_extract_p(text)
  effect <- score_extract_effect(text)
  se_reported <- score_extract_se(text)
  ci <- score_extract_ci(text)

  b <- effect$value
  se <- dplyr::coalesce(se_reported$value, ci$se)
  se_source <- case_when(
    !is.na(se_reported$value) ~ se_reported$source,
    !is.na(ci$se) ~ "ci_95",
    TRUE ~ "missing"
  )

  sign_info <- score_z_sign_from_context(text, b, effect$source)
  z_from_coef <- ifelse(!is.na(b) && !is.na(se) && se > 0, b / se, NA_real_)
  z_from_ci <- ifelse(
    !is.na(b) && !is.na(ci$se) && ci$se > 0,
    b / ci$se,
    NA_real_
  )

  z <- NA_real_
  z_operator <- NA_character_
  z_source <- "unparsed"
  sign_source <- sign_info$source

  if (!is.na(z_reported$value)) {
    z <- z_reported$value
    z_operator <- z_reported$operator
    z_source <- "reported_z"
    sign_source <- "reported_z"
  } else if (!is.na(t_reported$value)) {
    z <- t_reported$value
    z_operator <- t_reported$operator
    z_source <- "reported_t"
    sign_source <- "reported_t"
  } else if (!is.na(z_from_coef)) {
    z <- z_from_coef
    z_operator <- "="
    z_source <- "coef_over_se"
    sign_source <- effect$source
  } else if (
    !is.na(f_reported$value) &&
      !is.na(f_reported$df1) &&
      f_reported$df1 == 1
  ) {
    z <- sign_info$sign * sqrt(f_reported$value)
    z_operator <- f_reported$operator
    z_source <- "sqrt_f_df1_eq_1"
  } else if (!is.na(z_from_ci)) {
    z <- z_from_ci
    z_operator <- "="
    z_source <- "ci_95"
    sign_source <- effect$source
  } else if (!is.na(p_reported$value)) {
    p_for_z <- ifelse(p_reported$value == 0, 1e-300, p_reported$value)
    z <- sign_info$sign * z_from_two_sided_p(p_for_z)
    z_operator <- case_when(
      p_reported$value == 0 ~ ">",
      p_reported$operator == "<" ~ ">",
      p_reported$operator == ">" ~ "<",
      TRUE ~ "="
    )
    z_source <- "two_sided_p"
  }

  if (
    is.na(se) &&
      !is.na(b) &&
      !is.na(z) &&
      z != 0 &&
      z_operator == "=" &&
      z_source %in% c("reported_z", "reported_t")
  ) {
    se <- abs(b / z)
    se_source <- "effect_over_z"
  }

  parse_status <- case_when(
    !is.na(z) ~ "parsed_z",
    !is.na(b) | !is.na(se) | !is.na(p_reported$value) ~ "partial",
    TRUE ~ "unparsed"
  )

  tibble(
    measure = effect$measure,
    z = z,
    z_operator = z_operator,
    p = p_reported$value,
    p_operator = p_reported$operator,
    b = b,
    se = se,
    z_source = z_source,
    effect_source = effect$source,
    se_source = se_source,
    sign_source = sign_source,
    parse_status = parse_status
  )
}

derive_fragment_significance <- function(p, p_operator, z, z_operator) {
  case_when(
    !is.na(p) & p_operator == "=" ~ p <= 0.05,
    !is.na(p) & p_operator == "<" & p <= 0.05 ~ TRUE,
    !is.na(p) & p_operator == ">" & p >= 0.05 ~ FALSE,
    !is.na(p) ~ NA,
    !is.na(z) & z_operator == "=" ~ abs(z) >= 1.96,
    !is.na(z) & z_operator == ">" & abs(z) >= 1.96 ~ TRUE,
    !is.na(z) & z_operator == "<" & abs(z) < 1.96 ~ FALSE,
    TRUE ~ NA
  )
}

describe_significance_rule <- function(p, p_operator, z, z_operator) {
  case_when(
    !is.na(p) & p_operator == "=" ~ "exact_p",
    !is.na(p) & p_operator == "<" & p <= 0.05 ~ "p_upper_bound_significant",
    !is.na(p) & p_operator == ">" & p >= 0.05 ~
      "p_lower_bound_nonsignificant",
    !is.na(p) ~ "ambiguous_p_bound",
    !is.na(z) & z_operator == "=" ~ "exact_z",
    !is.na(z) & z_operator == ">" & abs(z) >= 1.96 ~
      "z_lower_bound_significant",
    !is.na(z) & z_operator == "<" & abs(z) < 1.96 ~
      "z_upper_bound_nonsignificant",
    !is.na(z) ~ "ambiguous_z_bound",
    TRUE ~ "unavailable"
  )
}

write_csv_if_present <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(as_tibble(data), path, na = "")
}

write_txt_table <- function(data, path, digits = 3) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  rounded <- data %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
  capture.output(print(rounded, n = nrow(rounded), width = Inf), file = path)
}
