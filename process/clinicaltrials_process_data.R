library(dplyr)
library(stringr)
eff <- readRDS("data_raw/clinicaltrials.gov/clinicaltrials.gov_aug2025.rds")


# Do minimal filtering and cleaning up first
eff <- eff %>% 
  rename(lower = ci_lower_limit, 
         upper = ci_upper_limit, 
         estimate = param_value) %>%
  mutate(across(c(lower, upper, estimate), as.numeric)) %>%
  # THIS IS THE MAIN FILTERING THAT WE DO FOR BEAR: COMPLETED STUDIES and PRIMARY OUTCOMES
  filter(
    overall_status == "COMPLETED",
    outcome_type == "PRIMARY"
  ) %>% 
  mutate(ci_percent = ifelse(ci_percent > 100, NA, ci_percent)) %>% 
  mutate(ci_percent = ifelse(ci_percent <   0, NA, ci_percent)) %>% 
  mutate(ci_percent = ifelse(ci_percent <   1, 100*ci_percent, ci_percent))

# --- helper: classify effect measures there are 100's of labels) ---

classify_measure <- function(x) {
  x0 <- str_squish(tolower(x))
  out <- case_when(
    # 1) Odds ratio
    str_detect(x0, "odds\\s*ratio") ~ "Odds Ratio",
    
    # 2) Risk ratio (a.k.a. relative risk)
    str_detect(x0, "(^|\\W)risk\\s*ratio(\\W|$)|relative\\s*risk") ~ "Risk Ratio",
    
    # 3) Hazard ratio (Cox or otherwise)
    str_detect(x0, "hazard\\s*ratio|cox\\s*proportional") ~ "Hazard Ratio",
    
    # 4) Mean difference (incl. LS/least-squares/adjusted variants)
    str_detect(x0, "(^|\\W)mean\\s*difference(\\W|$)") |
      str_detect(x0, "ls\\s*mean\\s*difference|least\\s*square[s]?\\s*mean\\s*difference|lsm\\s*difference") |
      str_detect(x0, "difference\\s*in\\s*ls\\s*means|difference\\s*of\\s*least\\s*squares\\s*means") |
      str_detect(x0, "adjusted\\s*mean\\s*difference|treatment\\s*difference$|vaccine\\s*group\\s*difference") |
      str_detect(x0, "difference\\s*\\(change\\s*from\\s*baseline\\)") ~ "Mean Difference",
    
    # Risk difference (incl. proportions/participants phrasing)
    str_detect(x0, "risk\\s*difference|(^|\\W)rd(\\W|$)|difference\\s*in\\s*proportions?") |
      str_detect(x0, "percentage\\s*of\\s*participants|difference\\s*in\\s*percentage\\s*of\\s*participants") ~
      "Risk Difference",
    
    # Difference in percentages (ambiguous relative change / percent points without “participants”)
    str_detect(x0, "percent(age)?\\s*difference|difference\\s*in\\s*percent(age|ages)?s?") ~
      "Difference in Percentages",
    
    # 7) Other ratio (geometric ratios, rate ratios, generic “ratio” after above)
    str_detect(x0, "rate\\s*ratio") ~ "Ratio",
    str_detect(x0, "geometric.*ratio|\\bgmr\\b|gmt\\s*ratio|gmc\\s*ratio") ~ "Geometric Ratio",
    str_detect(x0, "ratio\\s*of\\s*.*geometric|test/\\s*ref|t/\\s*r") ~ "Geometric Ratio",
    # catch-all "ratio" AFTER filtering specific ones above
    str_detect(x0, "(^|\\W)ratio(\\W|$)") ~ "Other Ratio",
    
    # everything else
    TRUE ~ "Other"
  )
  
  factor(out,
         levels = c("Mean Difference",
                    "Odds Ratio","Risk Ratio","Hazard Ratio", "Geometric Ratio",
                    "Risk Difference","Difference in Percentages",
                    "Ratio", "Other"))
}


#--- helper: z critical from CI level and sides ---
zcrit <- function(ci_percent, sides) {
  #Assuming 95% for NA actually makes no difference to results, as far as I saw
  ci_percent <- ifelse(is.na(ci_percent), 0.95, as.numeric(ci_percent)) 
  sides <- ifelse(is.na(sides) | !(sides %in% c(1,2)), 2L, as.integer(sides))
  alpha <- pmax(1 - ci_percent/100, .Machine$double.eps)
  ifelse(sides == 1L, qnorm(1 - alpha), qnorm(1 - alpha/2))
}

#--- build the z/effect/se table ---
z_table <- eff %>%
  # rename & coerce numerics
  mutate(across(c(lower, upper, estimate, ci_percent, ci_n_sides), function(x) suppressWarnings(as.numeric(x)))) %>%
  mutate(p_num_raw   = suppressWarnings(as.numeric(p_value))) %>%
  # keep plausible rows
  # filter(is.finite(estimate), is.finite(lower), is.finite(upper), upper > lower) %>%
  mutate(
    measure_class = classify_measure(param_type),
    log_ok      = (lower > 0 & estimate > 0 & upper > 0),
    scale       = ifelse(grepl("Ratio", measure_class) & log_ok, "log", "raw"),
    # This will return some warnings because we often have log of NA's
    est_x       = ifelse(scale == "log", log(estimate), estimate),
    lo_x        = ifelse(scale == "log", log(lower),    lower),
    hi_x        = ifelse(scale == "log", log(upper),    upper),
    
    # CI-derived SE and z (Wald-equivalent on chosen scale)
    z_crit      = zcrit(ci_percent, ci_n_sides),
    se_hi       = (hi_x - est_x) / z_crit,
    se_lo       = (est_x - lo_x) / z_crit,
    se_ci       = ifelse(is.finite(se_hi) & is.finite(se_lo),
                         (se_hi + se_lo)/2, ifelse(is.finite(se_hi), se_hi, se_lo)),
    z_ci        = est_x / se_ci,
    
    # symmetry check (if very asymmetric, CI may not be Wald on this scale)
    sym_ratio   = pmin(hi_x - est_x, est_x - lo_x) / pmax(hi_x - est_x, est_x - lo_x),
    
    # possible non-Wald CI types to distrust
    bad_type    = str_detect(tolower(param_type),
                             "median|hodges|posterior|bayes|exact|fieller|bootstrap|permutation"),
    
    # p-based z (two candidates: tail-p vs mislabelled CDF); pick one closest to CI
    p2_tail     = ifelse(between(p_num_raw, 0, 1), p_num_raw, NA_real_),
    p2_from_cdf = ifelse(between(p_num_raw, 0, 1), 2 * pmin(p_num_raw, 1 - p_num_raw), NA_real_),
    z_from_tail = ifelse(is.finite(p2_tail),
                         qnorm(1 - p2_tail/2), NA_real_),
    z_from_cdf  = ifelse(is.finite(p2_from_cdf),
                         qnorm(1 - p2_from_cdf/2), NA_real_),
    z_p_unsigned = case_when(
      is.finite(z_ci) & is.finite(z_from_tail) & is.finite(z_from_cdf) ~
        ifelse(abs(abs(z_from_tail) - abs(z_ci)) <= abs(abs(z_from_cdf) - abs(z_ci)),
               abs(z_from_tail), abs(z_from_cdf)),
      is.finite(z_from_tail) ~ abs(z_from_tail),
      is.finite(z_from_cdf)  ~ abs(z_from_cdf),
      TRUE                   ~ NA_real_
    ),
    # Modified in Dec 2025 to retain unsigned p-values
    z_sign_known = is.finite(est_x) & est_x != 0,
    z_sign       = ifelse(z_sign_known, sign(est_x), NA_real_),
    z_p          = ifelse(z_sign_known, z_sign * z_p_unsigned, z_p_unsigned),
    
    # choose best z:
    # prefer CI-based when it looks like a proper Wald CI on the chosen scale;
    # else fall back to p-based z-equivalent
    use_ci      = is.finite(z_ci) & is.finite(se_ci) & se_ci > 0 &
      !bad_type & (is.na(sym_ratio) | sym_ratio > 0.8),
    z_best      = ifelse(use_ci, z_ci, z_p),
    
    # make SE consistent with chosen z & effect
    se_best     = ifelse(use_ci, se_ci, ifelse(is.finite(z_best) & z_best != 0,
                                               abs(est_x) / abs(z_best), NA_real_))
  ) %>%
  # final tidy output (keep minimal join keys if you have them)
  mutate(effect = est_x, se = se_best, z = z_best) %>%
  mutate(year = lubridate::year(completion_date))

z_table_small <- z_table %>% 
  select(
    nct_id, brief_title, year, enrollment,
    allocation, study_type, phase, intervention_model, primary_purpose, masking,
    measure_class, scale,  
    effect, se, z, 
  ) %>% 
  filter(!is.na(z))

z_table_small %>%
  mutate_if(is.character, as.factor) %>%
  summary()

saveRDS(z_table_small, "data/clinicaltrialsgov.rds")
