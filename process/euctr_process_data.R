# data/eutrials/process_eutrials_data.R

library(dplyr)
library(stringr)
library(lubridate)

in_path  <- "data_raw/eutrials/data_euctr_ctgov.rds"
out_path <- "data/euctr.rds"

# --- helpers ---------------------------------------------------------------

norm_txt <- function(x) str_squish(tolower(ifelse(is.na(x), "", x)))

clean_level <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- ifelse(!is.na(x) & x > 100, NA_real_, x)
  x <- ifelse(!is.na(x) & x > 0 & x < 1, 100 * x, x) # if given as 0.95
  x
}

clean_side <- function(x) {
  x0 <- norm_txt(x)
  case_when(
    str_detect(x0, "one") ~ 1L,
    str_detect(x0, "two") ~ 2L,
    TRUE ~ 2L
  )
}

zcrit_from_level <- function(level, sides) {
  level <- ifelse(is.na(level), 95, level)
  sides <- ifelse(is.na(sides), 2L, sides)
  alpha <- pmax(1 - level/100, .Machine$double.eps)
  ifelse(sides == 1L, qnorm(1 - alpha), qnorm(1 - alpha/2))
}

classify_estimand <- function(x) {
  x0 <- norm_txt(x)
  case_when(
    x0 == "" ~ NA_character_,
    str_detect(x0, "odds\\s*ratio|\\bor\\b|oddsratio") ~ "Odds Ratio",
    str_detect(x0, "risk\\s*ratio|relative\\s*risk|\\brr\\b|riskratio") ~ "Risk Ratio",
    str_detect(x0, "hazard\\s*ratio|cox\\s*proportional|\\bhr\\b|hazardratio") ~ "Hazard Ratio",
    str_detect(x0, "rate\\s*ratio") ~ "Rate Ratio",
    str_detect(x0, "geometric.*mean.*ratio|\\bgmr\\b|\\bgmt\\b") ~ "Geometric Ratio",
    str_detect(x0, "(^|\\W)ratio(\\W|$)") ~ "Other Ratio",
    str_detect(x0, "mean.*difference|ls.*mean.*difference|least.*square.*mean|lsm") ~ "Mean Difference",
    str_detect(x0, "risk.*difference|(^|\\W)rd(\\W|$)|difference.*proportion") ~ "Risk Difference",
    str_detect(x0, "median.*difference") ~ "Median Difference",
    str_detect(x0, "slope") ~ "Slope",
    TRUE ~ "Other"
  )
}

is_log_label <- function(x) {
  x0 <- norm_txt(x)
  str_detect(x0, "\\.log\\b|\\blog\\b") | x0 %in% c("orl", "hrl", "rrl")
}

is_ratio_class <- function(measure_class) {
  measure_class %in% c("Odds Ratio","Risk Ratio","Hazard Ratio","Rate Ratio","Geometric Ratio","Other Ratio")
}

bad_ci_type <- function(x) {
  x0 <- norm_txt(x)
  str_detect(x0, "median|hodges|posterior|bayes|exact|bootstrap|permutation")
}

# --- main ------------------------------------------------------------------

d_raw <- readRDS(in_path)

d_clean <- d_raw %>%
  mutate(
    collection = toupper(collection),
    
    level_num  = clean_level(level),
    sides_num  = clean_side(side),
    zcrit      = zcrit_from_level(level_num, sides_num),
    
    p_raw      = coalesce(pval.calc, pval),
    p_num      = suppressWarnings(as.numeric(p_raw)),
    p_num      = ifelse(!is.na(p_num), pmin(pmax(p_num, .Machine$double.xmin), 1), NA_real_),
    
    trunc      = ifelse(is.na(trunc), "=", trunc),
    
    estimand_raw  = estimand,
    measure_class = classify_estimand(estimand_raw),
    ratio_like    = is_ratio_class(measure_class),
    already_log   = is_log_label(estimand_raw),
    
    scale = case_when(
      already_log ~ "log",
      ratio_like  ~ "log",
      TRUE        ~ "raw"
    ),
    
    estimate = suppressWarnings(as.numeric(estimate)),
    lower    = suppressWarnings(as.numeric(lower)),
    upper    = suppressWarnings(as.numeric(upper)),
    
    log_ok = !already_log & ratio_like &
      is.finite(estimate) & is.finite(lower) & is.finite(upper) &
      estimate > 0 & lower > 0 & upper > 0
  ) %>%
  
  # IMPORTANT: do log() only on the valid subset, outside mutate()
  { df <- .
  df$est_x <- ifelse(df$scale == "raw", df$estimate,
                     ifelse(df$already_log, df$estimate, NA_real_))
  df$lo_x  <- ifelse(df$scale == "raw", df$lower,
                     ifelse(df$already_log, df$lower, NA_real_))
  df$hi_x  <- ifelse(df$scale == "raw", df$upper,
                     ifelse(df$already_log, df$upper, NA_real_))
  
  idx <- which(df$scale == "log" & !df$already_log & df$log_ok)
  if (length(idx)) {
    df$est_x[idx] <- log(df$estimate[idx])
    df$lo_x[idx]  <- log(df$lower[idx])
    df$hi_x[idx]  <- log(df$upper[idx])
  }
  tibble::as_tibble(df)
  } %>%
  
  mutate(
    ci_ok = is.finite(est_x) & is.finite(lo_x) & is.finite(hi_x) &
      (hi_x > lo_x) & is.finite(zcrit) & zcrit > 0,
    
    se_hi = ifelse(ci_ok, (hi_x - est_x) / zcrit, NA_real_),
    se_lo = ifelse(ci_ok, (est_x - lo_x) / zcrit, NA_real_),
    se_ci = case_when(
      is.finite(se_hi) & is.finite(se_lo) ~ (se_hi + se_lo)/2,
      is.finite(se_hi) ~ se_hi,
      is.finite(se_lo) ~ se_lo,
      TRUE ~ NA_real_
    ),
    z_ci = ifelse(is.finite(se_ci) & se_ci > 0, est_x / se_ci, NA_real_),
    # This will also take care of cases where estimate is outside of interval
    sym_ratio = ifelse(ci_ok,
                       pmin(hi_x - est_x, est_x - lo_x) / pmax(hi_x - est_x, est_x - lo_x),
                       NA_real_
    ),
    
    z_p_abs = case_when(
      is.finite(p_num) & sides_num == 1L ~ qnorm(1 - p_num),
      is.finite(p_num) & sides_num == 2L ~ qnorm(1 - p_num/2),
      TRUE ~ NA_real_
    ),
    z_sign = ifelse(is.finite(est_x) & est_x != 0, sign(est_x), 1),
    z_p    = z_sign * z_p_abs,
    
    use_ci = is.finite(z_ci) & is.finite(se_ci) & se_ci > 0 &
      !bad_ci_type(estimand_raw) & (is.na(sym_ratio) | sym_ratio > 0.8),
    
    z = ifelse(use_ci, z_ci, z_p),
    b = ifelse(is.finite(est_x), est_x, NA_real_),
    se = case_when(
      use_ci ~ se_ci,
      is.finite(z) & z != 0 & is.finite(b) ~ abs(b) / abs(z),
      TRUE ~ NA_real_
    ),
    
    z_operator = case_when(
      trunc == "<" ~ ">",
      trunc == ">" ~ "<",
      TRUE ~ "="
    ),

    completion_date = as.Date(completion_date),
    year = year(as.Date(coalesce(completion_date, date)))
  ) %>%
  mutate(
    key0 = str_c(id, endpoint, estimand_raw, purpose, intervention, sep = "|"),
    key_n = ave(seq_along(key0), key0, FUN = seq_along),
    record_id = str_c(key0, key_n, sep = "|")
  ) %>%
  select(-key0, -key_n) %>%
  filter(is.finite(z)) %>% 
  # Make phase vector conform to the same naming convention as clinicaltrials.gov dataset
  mutate(
    phase = as.character(phase),
    phase = case_when(
      phase == "phase 1"       ~ "phase1",
      phase == "phase 1+2"     ~ "phase1/phase2",
      phase == "phase 2"       ~ "phase2",
      phase == "phase 2+3"     ~ "phase2/phase3",
      phase == "phase 3"       ~ "phase3",
      phase == "phase 4"       ~ "phase4",
      
      # these are rare/zero in your table; map to the closest available labels
      phase == "phase 1+2+3"   ~ "phase2/phase3",
      phase == "phase 3+4"     ~ "phase4",
      phase == "phase 2+4"     ~ "phase4",
      phase == "phase 1+2+3+4" ~ "phase4",
      
      TRUE ~ NA_character_
    )
  )

# For now I only use EUCTR, so clinicaltrials.gov is not even saved with clean data
d_clean  %>%
  filter(collection == "EUCTR") %>%
  saveRDS(out_path)
