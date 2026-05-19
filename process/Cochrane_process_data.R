library(tidyverse)
library(fs)
library(purrr)
library(stringr)
library(metafor)

# Review data + extracted data from each report
csv_path <- "data_raw/Cochrane/data/cdsr_interventions_19nov2025.csv"
coch_csv <- readr::read_csv(csv_path)
results_path <- "data_raw/Cochrane/data/cdsr_rm5_results.rds"
results_all <- readRDS(results_path)


# Some processing of results
results_all_fixed <- results_all %>%
  mutate(
    studies = map(studies, ~ {
      x <- .x
      # no study data: return empty tibble so unnest can drop this review
      if (is.null(x) || is.logical(x)) return(tibble())
      x <- as_tibble(x)
      x %>%
        mutate(
          across(
            any_of(c("subgroup.nr", "subgroup.id")),
            as.character
          )
        )
    })
  ) 


# meta-analysis data easy to unnest but unimportant
ma_long <- results_all_fixed %>%
  select(ma) %>%
  unnest(ma)

# Study year cleanup function
sanitize_study_year <- function(x, min_year = 1900, max_year = 2025) {
  y <- str_trim(x)
  y[y == ""] <- NA_character_
  
  out <- rep(NA_integer_, length(y))
  
  # Exact 4-digit year like "2005"
  plain <- !is.na(y) & str_detect(y, "^(19[0-9]{2}|20[0-2][0-9])$")
  out[plain] <- as.integer(y[plain])
  
  # For the rest, grab a plausible 4-digit year anywhere in the string
  remaining <- is.na(out) & !is.na(y)
  if (any(remaining)) {
    # last occurrence of 19xx or 20xx, e.g. "2005 Mar", "blah... 2006", "1998a"
    m <- str_match(y[remaining], ".*(19[0-9]{2}|20[0-2][0-9])\\D*$")[, 2]
    yr <- suppressWarnings(as.integer(m))
    yr[is.na(yr) | yr < min_year | yr > max_year] <- NA_integer_
    out[remaining] <- yr
  }
  out
}

classify_outcome_group <- function(comparison_name, outcome_name, subgroup_name) {
  txt <- str_c(
    coalesce(comparison_name, ""),
    coalesce(outcome_name, ""),
    coalesce(subgroup_name, ""),
    sep = " "
  ) %>% str_squish()
  
  bias <- str_detect(txt, regex(
    "bias|risk of bias|sensitivity analys|sensitivity analisys",
    ignore_case = TRUE
  ))
  drop_base <- str_detect(txt, regex(
    paste(
      "withdrawn|withdrawing|withdrew|withdrawal[s]*|withdrawl[s]*",
      "leaving\\s*[the]*\\s*(study|studies|trial|treatment)",
      "dropout[s]*|acceptability|tolerability",
      sep = "|"
    ),
    ignore_case = TRUE
  ))
  drop_exc <- str_detect(
    txt,
    regex(
      "alcohol withdrawal|withdrawal symptoms|steroid withdrawal",
      ignore_case = TRUE
    )
  )
  dropouts <- drop_base & !drop_exc
  safety <- str_detect(txt, regex(
    "toxicity|adverse|adverse effects|side effect|serious adverse|SAE",
    ignore_case = TRUE
  ))
  
  case_when(
    safety ~ "safety",
    dropouts ~ "dropouts",
    bias ~ "bias",
    TRUE ~ "efficacy"
  )
}


# Add flags for Cochrane meta-analyses that likely contained only RCTs

source("process/Cochrane_rct_score.R")
rct_flags <- classify_design_v2(coch_csv$Abstract)
coch_join <- coch_csv %>% 
  transmute(doi       = DOI,
            rct       = rct_flags$rct_only_high_conf,
            specialty = `Cochrane Review Group Code`)
   
                       
studies_long <- results_all_fixed %>%
  left_join(coch_join, by = "doi") %>% 
  # id in results_all is the same as the id column that's nested inside studies
  # (I double checked)
  select(cochrane_id, doi, specialty, rct, studies) %>% 
  unnest(studies) %>%
  mutate(study.year = sanitize_study_year(study.year)) %>% 
  mutate(
    id            = factor(id),
    outcome.nr    = suppressWarnings(as.integer(outcome.nr)),
    comparison.nr = suppressWarnings(as.integer(comparison.nr)),
    outcome.flag  = factor(outcome.flag),
    specialty     = factor(specialty),
    
    measure_group = factor(case_when(
      outcome.measure %in% c("RR", "Risk Ratio")              ~ "RR",
      outcome.measure %in% c("OR")                            ~ "OR",
      outcome.measure %in% c("PETO_OR")                       ~ "PETO_OR",
      outcome.measure %in% c("RD")                            ~ "RD",
      outcome.measure %in% c("MD", "Mean Difference")         ~ "MD",
      outcome.measure %in% c("SMD", "Std. Mean Difference")   ~ "SMD",
      TRUE                                                    ~ NA_character_
    )),
    
    # Recreate effect.* style columns from new fields
    z  = effect.size / se,
    outcome_group = factor(
      classify_outcome_group(comparison.name, outcome.name, subgroup.name),
      levels = c("efficacy", "safety", "dropouts", "bias")
    ),
    
    # Create these later:
    phase              = NA
  )


# Calculate the z-values using our preferred specification
# Probit for events (important!)
# And SMD for continuous data

studies_long <- dplyr::filter(studies_long, total1 > 0, total2 > 0)

# In 0.3% of data (or something like that) SMD cannot be calculated 
# because recorded SD is zero
# but only in 10 rows the effect size/SD is then non-zero
# (we still keep a lot of effect sizes == 0 after this procedure!)
cdsr <- rbind(
  dplyr::filter(studies_long, outcome.flag=="CONT") %>% 
    escalc(m1i=mean1,sd1i=sd1,n1i=total1,
           m2i=mean2,sd2i=sd2,n2i=total2,measure="SMD",
           data=., append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "SMD"),
  dplyr::filter(studies_long, outcome.flag=="DICH") %>% 
    escalc(ai=events1,n1i=total1,
           ci=events2,n2i=total2,measure="PBIT",
           data=.,append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "probit")
) %>% 
  mutate(z = yi/sqrt(vi))

saveRDS(cdsr, file = "data/Cochrane.rds")
