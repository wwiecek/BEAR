library(dplyr)
library(stringr)
library(readr)

# We previously used a data cut that was processed by Kraft/Mead (2023)
# dtlist[["WWC"]] <- read_csv("data/WWC/Kraft_wwc_merge.csv", show_col_types = FALSE) %>% 
#   mutate(pval = as.numeric(f_p_Value_WWC),
#          b = as.numeric(Effect.size)) %>% 
#   # For some p-values the database records p = 0, so z = Inf, but we can "rescue"
#   # some information by setting it to the smallest value in WWC dataset:
#   mutate(pval = ifelse(pval == 0, 2e-16, pval)) %>%
#   mutate(pval = ifelse(is.na(pval), f_p_Value_Any, pval)) %>% 
#   transmute(
#     metaid = NA, 
#     studyid = cite_trunc,
#     method = NA,
#     measure = NA,
#     z = sign(b)*z_from_p(pval),
#     b = b,
#     se = NA,
#     ss = Sample.size,
#     year = Publication.year) 



# WWC source directly
wwc_findings <- read_csv("data_raw/WWC/WWC_17May2025/Findings.csv")
wwc_intrep   <- read_csv("data_raw/WWC/WWC_17May2025/InterventionReports.csv")
wwc_revdict  <- read_csv("data_raw/WWC/WWC_17May2025/ReviewDictionary.csv")
wwc_studies  <- read_csv("data_raw/WWC/WWC_17May2025/Studies.csv")


# two-sided p -> |z|
z_from_p <- function(p) qnorm(p / 2, lower.tail = FALSE)

wwc_cleaned <- wwc_findings %>%
  inner_join( transmute(wwc_studies,
                        ReviewID,
                        StudyID,
                        Citation,
                        Publication_Date,
                        Study_Design,
                        Study_Rating), 
              by = "ReviewID") %>%  
  mutate(
    method = case_when(
      Study_Design %in% c("Randomized controlled trial", "Randomized Controlled Trial") ~ "RCT",
      Study_Design == "Quasi-Experimental Design" ~ "quasi",
      is.na(Study_Design) ~ "unknown",
      TRUE ~ "other"
    ),
    
    # effect size: prefer WWC, fallback to Study
    b = coalesce(Effect_Size_WWC, Effect_Size_Study),
    
    # p-values: prefer WWC, fallback to Study
    pval = coalesce(p_Value_WWC, p_Value_Study),
    pval = as.numeric(pval),
    pval = if_else(pval >= 0 & pval <= 1, pval, NA_real_),
    pval = if_else(pval == 0, 1e-16, pval), #1e-16 is the smallest reported value in the set
    
    # sample size: prefer total, else sum arms
    ss = coalesce(
      Outcome_Sample_Size,
      Outcome_Measure_Intervention_Sample_Size + Outcome_Measure_Comparison_Sample_Size
    ),
    
    # publication year: grab a 4-digit year if present
    year = as.integer(str_extract(Publication_Date, "\\d{4}")),
    # truncating strings would not work here, mind, so it's best to just make our own IDs, 
    # even if we lose some information
    study_id = as.character(as.numeric(factor(Citation)))
  ) 

saveRDS(wwc_cleaned, "data/WWC.rds")
