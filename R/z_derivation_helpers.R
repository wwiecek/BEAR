# Shared z-derivation helpers for trial-style p-value and CI data.

# Normalise labels before regex-based measure and CI parsing.
norm_trial_text <- function(x) {
  stringr::str_squish(tolower(ifelse(is.na(x), "", as.character(x))))
}

# Clean CI levels to percentages; optionally fill dataset-specific defaults.
clean_ci_level <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x))
  out <- ifelse(!is.na(out) & out > 100, NA_real_, out)
  out <- ifelse(!is.na(out) & out < 0, NA_real_, out)
  out <- ifelse(!is.na(out) & out > 0 & out < 1, 100 * out, out)
  ifelse(is.na(out), default, out)
}

# Convert reported side labels to one- or two-sided tests/intervals.
clean_ci_sides <- function(x, default = 2L) {
  numeric_sides <- suppressWarnings(as.integer(x))
  text <- norm_trial_text(x)
  out <- dplyr::case_when(
    !is.na(numeric_sides) & numeric_sides %in% c(1L, 2L) ~ numeric_sides,
    stringr::str_detect(text, "one") ~ 1L,
    stringr::str_detect(text, "two") ~ 2L,
    TRUE ~ as.integer(default)
  )
  as.integer(out)
}

# Critical value implied by CI level and one-/two-sided interval reporting.
ci_critical_value <- function(level, sides) {
  level <- ifelse(is.na(level), 95, as.numeric(level))
  sides <- ifelse(is.na(sides) | !(sides %in% c(1L, 2L)), 2L, as.integer(sides))
  alpha <- pmax(1 - level / 100, .Machine$double.eps)
  ifelse(sides == 1L, qnorm(1 - alpha), qnorm(1 - alpha / 2))
}

# Map noisy trial estimand labels to broad BEAR measure classes.
classify_trial_measure <- function(x) {
  raw <- stringr::str_squish(ifelse(is.na(x), "", as.character(x)))
  x0 <- norm_trial_text(x)
  dplyr::case_when(
    x0 == "" ~ NA_character_,
    stringr::str_detect(x0, "odds\\s*ratio|oddsratio") |
      x0 %in% c("or", "orl") | stringr::str_detect(raw, "\\bOR\\b") ~ "Odds Ratio",
    stringr::str_detect(x0, "risk\\s*ratio|relative\\s*risk|riskratio") |
      x0 %in% c("rr", "rrl") | stringr::str_detect(raw, "\\bRR\\b") ~ "Risk Ratio",
    stringr::str_detect(x0, "hazard\\s*ratio|cox\\s*proportional|hazardratio") |
      x0 %in% c("hr", "hrl") | stringr::str_detect(raw, "\\bHR\\b") ~ "Hazard Ratio",
    stringr::str_detect(x0, "rate\\s*ratio") ~ "Rate Ratio",
    stringr::str_detect(x0, "geometric.*ratio|geometric.*mean.*ratio|\\bgmr\\b|gmt\\s*ratio|gmc\\s*ratio|\\bgmt\\b") ~ "Geometric Ratio",
    stringr::str_detect(x0, "ratio\\s*of\\s*.*geometric|test/\\s*ref|t/\\s*r") ~ "Geometric Ratio",
    stringr::str_detect(x0, "(^|\\W)ratio(\\W|$)") ~ "Other Ratio",
    stringr::str_detect(x0, "(^|\\W)mean\\s*difference(\\W|$)|mean\\s*diff|meandiff|ls\\s*mean\\s*difference|least\\s*square[s]?\\s*mean|lsm") |
      stringr::str_detect(x0, "difference\\s*in\\s*ls\\s*means|difference\\s*of\\s*least\\s*squares\\s*means|adjusted\\s*mean\\s*difference|treatment\\s*difference$|vaccine\\s*group\\s*difference|difference\\s*\\(change\\s*from\\s*baseline\\)") ~ "Mean Difference",
    stringr::str_detect(x0, "risk\\s*difference|(^|\\W)rd(\\W|$)|difference\\s*in\\s*proportions?|difference.*proportion") |
      stringr::str_detect(x0, "percentage\\s*of\\s*participants|difference\\s*in\\s*percentage\\s*of\\s*participants") ~ "Risk Difference",
    stringr::str_detect(x0, "percent(age)?\\s*difference|difference\\s*in\\s*percent(age|ages)?s?") ~ "Difference in Percentages",
    stringr::str_detect(x0, "median.*difference|median\\s*diff|mediandiff") ~ "Median Difference",
    stringr::str_detect(x0, "slope") ~ "Slope",
    TRUE ~ "Other"
  )
}

# Ratio measures are analysed on the log scale when values allow it.
is_ratio_measure <- function(measure_class) {
  measure_class %in% c(
    "Odds Ratio", "Risk Ratio", "Hazard Ratio", "Rate Ratio",
    "Geometric Ratio", "Other Ratio", "Ratio"
  )
}

# EUCTR sometimes flags already-log estimates in the estimand label.
is_log_measure_label <- function(x) {
  x0 <- norm_trial_text(x)
  stringr::str_detect(x0, "\\.log\\b|\\blog\\b") | x0 %in% c("orl", "hrl", "rrl")
}

# Labels that suggest CIs should not be treated as simple Wald intervals.
is_non_wald_ci_label <- function(x) {
  stringr::str_detect(
    norm_trial_text(x),
    "median|hodges|posterior|bayes|exact|fieller|bootstrap|permutation"
  )
}

# Convert p-value bounds to the corresponding absolute-z bound direction.
z_operator_from_p_operator <- function(p_operator) {
  op <- as.character(p_operator)
  dplyr::case_when(
    is.na(op) ~ "=",
    op %in% c("<", "<=", "≤") ~ ">",
    op %in% c(">", ">=", "≥") ~ "<",
    TRUE ~ "="
  )
}

# P-value to z-value using reported sidedness, defaulting to two-sided.
z_from_p_value <- function(p, sides = 2L) {
  p <- suppressWarnings(as.numeric(p))
  p <- ifelse(!is.na(p) & p >= 0 & p <= 1, p, NA_real_)
  sides <- clean_ci_sides(sides)
  ifelse(sides == 1L, qnorm(1 - p), qnorm(1 - p / 2))
}

# Derive candidate CI and p-value z-statistics, then choose CI if trusted.
derive_trial_z <- function(estimate, lower, upper, p_value, p_operator = NULL,
                           ci_level = NULL, ci_sides = NULL, p_sides = NULL,
                           measure_label = NULL, default_ci_level = NA_real_,
                           already_log = NULL) {
  n <- length(estimate)
  if (is.null(p_operator)) p_operator <- rep("=", n)
  if (is.null(ci_level)) ci_level <- rep(NA_real_, n)
  if (is.null(ci_sides)) ci_sides <- rep(2L, n)
  if (is.null(p_sides)) p_sides <- rep(2L, n)
  if (is.null(measure_label)) measure_label <- rep(NA_character_, n)
  if (is.null(already_log)) already_log <- is_log_measure_label(measure_label)

  estimate <- suppressWarnings(as.numeric(estimate))
  lower <- suppressWarnings(as.numeric(lower))
  upper <- suppressWarnings(as.numeric(upper))
  p_num <- suppressWarnings(as.numeric(p_value))
  p_num <- ifelse(!is.na(p_num) & p_num >= 0 & p_num <= 1, p_num, NA_real_)
  p_operator <- ifelse(is.na(p_operator), "=", as.character(p_operator))
  ci_level <- clean_ci_level(ci_level, default = default_ci_level)
  ci_sides <- clean_ci_sides(ci_sides)
  p_sides <- clean_ci_sides(p_sides)
  z_crit <- ci_critical_value(ci_level, ci_sides)

  measure_class <- classify_trial_measure(measure_label)
  ratio_like <- is_ratio_measure(measure_class)
  log_ok <- ratio_like & !already_log &
    is.finite(estimate) & is.finite(lower) & is.finite(upper) &
    estimate > 0 & lower > 0 & upper > 0
  scale <- ifelse(already_log | (ratio_like & log_ok), "log", "raw")

  est_x <- estimate
  lo_x <- lower
  hi_x <- upper
  log_idx <- scale == "log" & !already_log & log_ok
  est_x[log_idx] <- log(estimate[log_idx])
  lo_x[log_idx] <- log(lower[log_idx])
  hi_x[log_idx] <- log(upper[log_idx])
  bad_log_idx <- scale == "log" & !already_log & !log_ok
  est_x[bad_log_idx] <- NA_real_
  lo_x[bad_log_idx] <- NA_real_
  hi_x[bad_log_idx] <- NA_real_

  ci_ok <- is.finite(est_x) & is.finite(lo_x) & is.finite(hi_x) &
    hi_x > lo_x & is.finite(z_crit) & z_crit > 0
  se_hi <- ifelse(ci_ok, (hi_x - est_x) / z_crit, NA_real_)
  se_lo <- ifelse(ci_ok, (est_x - lo_x) / z_crit, NA_real_)
  se_ci <- dplyr::case_when(
    is.finite(se_hi) & is.finite(se_lo) ~ (se_hi + se_lo) / 2,
    is.finite(se_hi) ~ se_hi,
    is.finite(se_lo) ~ se_lo,
    TRUE ~ NA_real_
  )
  z_ci <- ifelse(is.finite(se_ci) & se_ci > 0, est_x / se_ci, NA_real_)
  sym_ratio <- ifelse(
    ci_ok,
    pmin(hi_x - est_x, est_x - lo_x) / pmax(hi_x - est_x, est_x - lo_x),
    NA_real_
  )

  p_tail <- p_num
  p_cdf <- ifelse(
    !is.na(p_num) & p_sides == 1L,
    pmin(p_num, 1 - p_num),
    ifelse(!is.na(p_num), 2 * pmin(p_num, 1 - p_num), NA_real_)
  )
  z_from_tail <- z_from_p_value(p_tail, p_sides)
  z_from_cdf <- z_from_p_value(p_cdf, p_sides)
  z_p_unsigned <- dplyr::case_when(
    is.finite(z_ci) & !is.na(z_from_tail) & !is.na(z_from_cdf) &
      abs(abs(z_from_tail) - abs(z_ci)) <=
        abs(abs(z_from_cdf) - abs(z_ci)) ~ abs(z_from_tail),
    is.finite(z_ci) & !is.na(z_from_tail) & !is.na(z_from_cdf) ~ abs(z_from_cdf),
    !is.na(z_from_tail) ~ abs(z_from_tail),
    !is.na(z_from_cdf) ~ abs(z_from_cdf),
    TRUE ~ NA_real_
  )
  z_sign_known <- is.finite(est_x) & est_x != 0
  z_sign <- ifelse(z_sign_known, sign(est_x), NA_real_)
  z_p <- ifelse(z_sign_known, z_sign * z_p_unsigned, z_p_unsigned)

  bad_ci_type <- is_non_wald_ci_label(measure_label)
  use_ci <- is.finite(z_ci) & is.finite(se_ci) & se_ci > 0 &
    !bad_ci_type & (is.na(sym_ratio) | sym_ratio > 0.8)
  z <- ifelse(use_ci, z_ci, z_p)
  b <- ifelse(is.finite(est_x), est_x, NA_real_)
  se <- dplyr::case_when(
    use_ci ~ se_ci,
    !is.na(z) & z != 0 & is.finite(b) ~ abs(b) / abs(z),
    TRUE ~ NA_real_
  )
  z_source <- dplyr::case_when(
    use_ci ~ "ci",
    !is.na(z_p) ~ "p",
    TRUE ~ NA_character_
  )
  z_operator <- ifelse(use_ci, "=", z_operator_from_p_operator(p_operator))

  tibble::tibble(
    measure_class, ratio_like, already_log, scale, log_ok,
    ci_level, ci_sides, p_sides, z_crit, p_num, z_ci, z_p, z_source, use_ci,
    b, se, z, sym_ratio, z_operator
  )
}
