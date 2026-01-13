library(tidyverse)
library(cochrane)
library(fs)
library(purrr)
library(stringr)
library(metafor)

# Review data + extracted data from each report
csv_path <- "data_raw/Cochrane/cdsr_interventions_19nov2025.csv"
coch_csv <- readr::read_csv(csv_path)
results_path <- "data_raw/Cochrane/cdsr_rm5_results.rds"
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


# Add flags for Cochrane meta-analyses that likely contained only RCTs

source("data_raw/Cochrane/cdsr_rct_score.R")
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
    
    # Create these later:
    phase              = NA
  )

saveRDS(studies_long, file = "data/Cochrane.rds")
