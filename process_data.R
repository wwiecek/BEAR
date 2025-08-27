# Prepare data -----
library(haven)
library(tidyverse)
library(metafor) #for CDSR calculations of effect sizes (binary data)
library(stringi) #for cleaning up some study names

dt_list <- list()

# Brodeur et al ------

dt_list[["Brodeur"]] <- read_dta("data/Brodeur/merged.dta") %>% 
  # filter(method=="RCT") %>%
  transmute(
    metaid = NA,
    studyid = title,
    method = method,
    measure = NA,
    z = myz,
    b = coeff,
    se = stderror,
    year = year)

# Could use:
# mutate(
# prereg = factor(prereg, labels=c("not pre-registered","pre-registered")), 
# papregistry = factor(papregistry,labels=c("no analysis plan","analysis plan"))) %>% 



# Askarov -----

# data from https://github.com/anthonydouc/Datasharing/blob/master/Stata/Mandatory%20data-sharing%2030%20Aug%202022.dta

dt_list[["Askarov"]] <- read_dta("data/Askarov/Mandatory data-sharing 30 Aug 2022.dta") %>% 
  mutate(z = effectsize/standarderror) %>% 
  dplyr:: filter(Excludegroup==0) %>% 
  transmute(
    metaid = filename,
    studyid = studyid,
    method = ifelse(EXPERIMENT == 1, "RCT", ifelse(mixed == 1, "mixed", "observational")),
    measure = NA,
    z = effectsize/standarderror,
    b = effectsize,
    se = standarderror,
    year = ifelse(YEARintervention != 0, YEARintervention, NA))

# could be useful:
# mutate(data_sharing = (EVENT==0 | INTERVENTION==1)) %>% 



# Arel-Bundock -----

# dataset from https://osf.io/fgdet
# requires running make to obtain one of the datasets estimates_rb_vab_mma.csv
# and them get_data.R to replicate the approach of authors to creating a single estimates.csv

# "We examine statistical power in political science by assembling a dataset of 16,649
# hypothesis tests, grouped in 351 meta-analyses, reported in 46 peer-reviewed
# meta-analytic articles"

dt_list[["ArelBundock"]] <- 
  read_csv("data/ArelBundock/estimates.csv", 
           show_col_types = FALSE) %>% 
  # There may be problems printing and encoding some characters down the line
  # e.g. "unable to translate 'Garc<cd>a Bedolla and Michelson' to a wide string"
  # so best to normalise them
  mutate(study_id = stri_trans_nfc(study_id)) %>% 
  transmute(
    metaid = meta_id, 
    studyid = study_id,
    field = subfield,
    method = NA,
    measure = NA,
    z = as.numeric(estimate)/as.numeric(std.error),
    b = as.numeric(estimate),
    se = as.numeric(std.error),
    ss = n,
    year = study_year) 



# What Works Clearinghouse -----

# Erik:
# The data about educational studies were downloaded by Chris Mead from the 
# “What Works Clearinghouse” on December 29, 2023. The website is 
# https://ies.ed.gov/ncee/WWC/StudyFindings 

dt_list[["WWC"]] <- read_csv("data/WWC/Kraft_wwc_merge.csv", show_col_types = FALSE) %>% 
  mutate(pval = as.numeric(f_p_Value_WWC),
         b = as.numeric(Effect.size)) %>% 
  # For some p-values the database records p = 0, so z = Inf, but we can "rescue"
  # some information by setting it to the smallest value in WWC dataset:
  mutate(pval = ifelse(pval == 0, 2e-16, pval)) %>%
  mutate(pval = ifelse(is.na(pval), f_p_Value_Any, pval)) %>% 
  transmute(
    metaid = NA, 
    studyid = cite_trunc,
    method = NA,
    measure = NA,
    z = sign(b)*qnorm(1 - pval/2),
    b = b,
    se = NA,
    ss = Sample.size,
    year = Publication.year) 

# potentially also of interest:
# read_csv("data/WWC/Kraft_wwc_merge.csv") %>% pull(Academic.subject) %>% table



# Yang et al -----

# Datafile dat_processed_rob.csv may be downloaded from 
# https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/sensitivity/dat_processed_rob.csv

extract_year_yang <- function(x) {
  # Find the last 4 consecutive digits in the string
  year_match <- regexpr("\\d{4}[^0-9]*$", x)
  if (year_match > 0) {
    potential_year <- as.numeric(substr(x, year_match, year_match + 3))
    if (potential_year >= 1950 && potential_year <= 2025) {
      return(potential_year)
    }
  }
  return(NA)
}

# this derived dataset is OK, but it does not have year, method, and meta_id
# that I'd like to use, therefore I do minimal data processing in data/Yang/
# to get data from Yang 2023 paper

# dt_list[["Yang"]] <- read.csv("data/Yang/dat_processed_rob.csv") %>% 
#   transmute(
#     metaid = NA, 
#     studyid = study,
#     method = NA,
#     z = z,
#     b = es,
#     se = se) %>%
#   mutate(year = sapply(studyid, extract_year_yang))

load("data/Yang/EcoEvo_PB_datasets.Rdata")

dt_list[["Yang"]] <- rbind(
  lnRR %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "lnRR"),
  
  Zr %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "Zr"),
  
  SMD %>% 
    lapply(function(x) select(x, study_ID, year_pub, es, sei) %>% 
             mutate(study_ID = as.character(study_ID))) %>% 
    bind_rows(.id = "meta_id") %>% 
    mutate(measure = "SMD")
) %>% 
  mutate(year_pub = ifelse(year_pub < 1900, NA, year_pub)) %>% # 1 typo
  transmute(
      metaid = meta_id,
      studyid = study_ID,
      year = year_pub,
      measure = measure,
      method = NA,
      z = es/sei,
      b = es,
      se = sei,
      ss = NA)


# Costello and Fox -----

# Data from @costello2022decline which were further processed by @yang2024large 
# and made available at 
# [Github](https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/main/main_dat_processed.csv)

dt_list[["CostelloFox"]] <- 
  read.csv("data/Yang/main_dat_processed.csv") %>% 
  transmute(
    metaid = as.character(meta.analysis.id), #meta.analysis.paper has only 232 unique values, this has 466
    studyid = study2,
    measure = grouped_es, #could use eff.size.measure for more info
    method = NA,
    z = z,
    b = eff.size,
    se = se.eff.size,
    ss = NA,
    year = study.year) 

# # group_by(key) %>% 
# # # Erik chose one 
# # slice_sample(n = 1) 



# Jager and Leek -----

load("data/JagerLeek/pvalueData.rda")

dt_list[["JagerLeek"]] <-  
  pvalueData %>% 
  data.frame() %>% 
  select(-abstract) %>% 
  mutate_at(c('pvalue','year'), as.numeric) %>%
  mutate(method = ifelse(grepl("randomized|randomised|controlled", title), "RCT", NA)) %>% 
  transmute(
    metaid = NA,
    studyid = pubmedID,
    method = method,
    measure = NA,
    p = pvalue,
    z = -qnorm(pvalue/2),
    b = NA, se = NA, ss=NA,
    # about 1/3 truncated, almost always .0001, .001, .01, or .05, so it's "p <"
    z_operator = ifelse(pvalueTruncated == "1", ">", "="),
    year = year) 



# Sladekova -----

dfs <- lapply(list.files("data/Sladekova/", full.names = TRUE), read.csv)
names(dfs) <- list.files("data/Sladekova/")



# sort(table(unlist(lapply(dfs, function(f) names(f)))), decreasing = TRUE)

dt_list[["Sladekova"]] <- lapply(dfs, function(df){
  
  if(all(c("n_treatment", "n_control") %in% colnames(df))) df$n <- df$n_treatment + df$n_control
  
  ret <- df %>% 
    filter(!is.na(yi) & !is.na(vi)) %>% 
    # Fisher's z won't work otherwise; this affects less than 0.5% of observations
    filter((1 + yi)/(1 - yi) > 0) %>% 
    mutate(
      b = 0.5*log((1 + yi)/(1 - yi)),
      se = sqrt(vi)) 
  
  # Trying to count sample sizes where possible
  
  if(nrow(ret) == 0)
    return(data.frame())
  # Study column is broken, as a temporary fix I need to assign new numerical study IDs
  # if(!is.null(ret$author)) ret$studyid <- as.character(ret$author) else ret$studyid <- as.character(1:nrow(ret))
  if(is.null(ret$year)) ret$year <- as.numeric(NA) else ret$year <- as.numeric(ret$year)
  if(is.null(ret$n)) ret$ss <- as.numeric(NA) else ret$ss <- as.numeric(ret$n)
  
  ret %>% 
    select(b, se, year, ss)
}) %>% 
  bind_rows(.id = "metaid") %>% 
  filter(!is.na(b) & !is.infinite(b)) %>% 
  mutate(studyid = 1:nrow(.)) %>% 
  mutate(method = NA,
         measure = "Zr",
         z = b/se,
         year = NA) %>% 
  filter(!is.na(metaid)) 

# year and studyid could be extracted, but they would need some cleaning up of strings
# in each of the 406 files, so it may be best to do it later



# Metapsy -----

sort(table(unlist(lapply(readRDS("data/Metapsy/metapsy_dt.rds"), function(f) names(f)))), decreasing = TRUE)

dt_list[["Metapsy"]] <- readRDS("data/Metapsy/metapsy_dt.rds") %>% 
  lapply(function(df) {
    if(is.null(df$.g)) {
      # for total-response dataset, I calculate log(OR) and convert to SMD
      # (there is a shorter way to do it, but I didn't notice initially that
      #  there are raw counts in this dataset)
      df <- df %>% 
        mutate(logor =     plogit.ig - plogit.cg,
               se.logor =  se.plogit.ig^2 + se.plogit.cg^2) %>% 
        mutate(.g  = logor/(pi/sqrt(3)),
               .g_se = se.logor/(pi/sqrt(3)))
    }
    
    if(all(c("n_arm1", "n_arm2") %in% colnames(df))) df$ss <- df$n_arm1 + df$n_arm2
    else df$ss <- NA
    
    if (all(c("study", ".g", ".g_se") %in% colnames(df))) {
      ret <- df[, c("study", ".g", ".g_se", "ss"), drop = FALSE]
      if(!is.null(df$year))
        ret$year <- df$year
      else #this happens in suicide-psyctr
        ret$year <- as.numeric(sub(".*?(\\d{4}).*?$", "\\1", df$study))
      return(ret)
    } else {
      return(data.frame())
    }
  }) %>% 
  bind_rows(.id = "metaid") %>% 
  transmute(
    metaid = metaid,
    studyid = study,
    method = "RCT",
    measure = "g",
    year = year,
    z = .g/.g_se,
    b = .g,
    se = .g_se,
    ss = ss)



# Barnett and Wren -----

load("data/BarnettWren/Georgescu.Wren.RData")

dt_list[["BarnettWren"]] <- complete %>% 
  filter(!mistake) %>% 
  # 0.3% of available values have CI widths other than 95%, let's remove these
  # but if ci.level is unknown, assume that it's actually 95% 
  mutate(ci.level = ifelse(is.na(ci.level), 0.95, ci.level)) %>% 
  filter(ci.level == 0.95) %>% 
  # Remove cases where CI is zero
  filter(lower < upper) %>% 
  # To allow for log(lower), add a tiny value to zeroes (~0.1% of the sample)
  mutate(lower = ifelse(lower > 0, lower, 1e-05)) %>%
  mutate(se = (log(upper) - log(lower))/(2*1.96)) %>% 
  mutate(b = (log(upper) + log(lower))/2) %>% 
  mutate(z = b/se) %>% 
  transmute(metaid = NA,
            studyid = pubmed,
            method = NA,
            measure = "ratio",
            z = b/se,
            b = b,
            se = se,
            year = Year,
            ss = NA)


# Cochrane -----

load("data/Cochrane/CDSR.RData")  # reads in dataframe "data"
data_filtered <- data %>% 
  select(-char.interventions) %>% 
  filter(
    # non-RCT data comprise more than half of all data
    # RCT=="yes",
    # small minority of data is IPD or IV ("results with effects and standard errors 
    # but without the data necessary for their computation") and we exclude these
    outcome.flag %in% c("CONT","DICH"),
    outcome.group=="efficacy",
    outcome.nr==1,
    comparison.nr==1)

dt_list[["Cochrane"]] <- rbind(
  dplyr::filter(data_filtered, outcome.flag=="CONT") %>% 
    escalc(m1i=mean1,sd1i=sd1,n1i=total1,
           m2i=mean2,sd2i=sd2,n2i=total2,measure="SMD",
           data=., append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "SMD"),
  dplyr::filter(data_filtered, outcome.flag=="DICH") %>% 
    escalc(ai=events1,n1i=total1,
           ci=events2,n2i=total2,measure="PBIT",
           data=.,append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "probit")
) %>% 
  transmute(
    metaid = id,
    studyid = study.name,
    method = ifelse(RCT == "yes", "RCT", "observational"),
    measure = measure,
    z = yi/sqrt(vi),
    b = yi,
    se = sqrt(vi),
    year = study.year,
    ss = total1 + total2)

rm(data, data_filtered)



# Adda -----

# dt_list[["Adda"]] <- read_dta("data/Adda/data_counterfactual_analysis.dta") %>% 
#   # 15 cases of trials that will finish in the future, so set to NA just in case
#   mutate(ifelse(completion_year >= 2025, NA, completion_year)) %>% 
#   filter(phase %in% c("Phase 2","Phase 3")) %>% 
#   transmute(metaid = NA,
#             studyid = nct_id,
#             method = "RCT",
#             measure = NA,
#             year = completion_year,
#             z = z,
#             b = NA,
#             se = NA,
#             ss = enrollment, #remember to note this in dataset notes!
#             z_operator = case_when(p_value_modifier == "<" ~ ">",
#                                    p_value_modifier == ">" ~ "<",
#                                    .default = "=")
#   )

dt_list[["clinicaltrials"]] <- readRDS("data/clinicaltrials.gov/data_cut_with_z.rds") %>% 
  filter(study_type == "INTERVENTIONAL" & allocation == "RANDOMIZED") %>% 
  # Remove some studies with huge numbers of arms; they are typically phase 1 or 2 studies
  # that are just throwing darts at many outcomes, so let's keep more ideal outcomes
  group_by(nct_id) %>% filter(n() < 20) %>% ungroup %>% 
  transmute(studyid = nct_id,
            year = year,
            group = tolower(phase),
            method = "RCT",
            measure = measure_class,
            z = z,
            b = effect,
            se = se,
            ss = enrollment,
            z_operator = "=")

# Head -----

dt_list[["Head"]] <- readRDS("data/Head/head.rds") %>% 
  transmute(metaid = NA,
            studyid = pmid,
            source = section,
            method = NA,
            measure = NA,
            year = year,
            p = p.value,
            z = qnorm(1 - p.value/2),
            b = NA,
            se = NA,
            ss = NA,
            # we do not differntiate between leq and lesser, because it doesn't 
            # really change analytical procedures we have in mind
            z_operator = case_when(
              operator == "=" ~ "=",
              operator == "<" ~ ">",
              operator == ">" ~ "<",
              operator == "≤" ~ ">",
              operator == "≥" ~ "<"
            )) 



# Chavalarias -----

dt_list[["Chavalarias"]] <- readRDS("data/Chavalarias/chavalarias.rds") %>% 
  # p_format coding is explained on Harvard Dataverse, but 99.3% of the values are "plain", 
  # so we stick to that
  filter(p_format == "plain") %>% 
  mutate(operator = case_when(
    operator %in% c("<", "<<", "<<<", "<=", "less than", "=<") ~ "<",
    operator == "=" ~ "=",
    operator == ">" ~ ">",
    TRUE ~ NA)) %>%
  # removes <0.08% of data with some edge cases we are not interested in
  filter(!is.na(operator)) %>% 
  transmute(metaid = NA,
            studyid = studyid,
            source = source,
            method = NA,
            measure = NA,
            year = year,
            p = p,
            z = qnorm(1 - p/2),
            b = NA,
            se = NA,
            ss = NA,
            z_operator = case_when(
              operator == "=" ~ "=",
              operator == "<" ~ ">",
              operator == ">" ~ "<"
            )) 



# Bogdan -----

# x <- read_csv("data/Bogdan/df_combined_pruned_Jan21.csv")
# x <- read_csv("data/Bogdan/df_combined_w_no_sig_all_aff_Jan21.csv")
# x <- read_csv("data/Bogdan/Youyou_etal_replications.csv")
# View(x)
# 
# dt_list[["Bogdan"]] <- x

# Processing of individual datasets and binding into a single large dataset ----
bear <- dt_list %>% 
  lapply(function(x) {x$studyid <- as.character(x$studyid); x}) %>% 
  lapply(filter, !is.na(z)) %>% 
  bind_rows(.id = "dataset") %>% 
  group_by(dataset, metaid, studyid) %>% 
  # mutate(k = n()) %>% 
  ungroup() %>% 
  group_by(dataset, metaid) %>% 
  ungroup()

saveRDS(bear, "data/BEAR.rds")

# Refresh README
rmarkdown::render("README.Rmd", output_format = "github_document")
