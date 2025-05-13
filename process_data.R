# Prepare data -----
library(haven)
library(tidyverse)

dt_list <- list()

# Brodeur et al ------

dt_list[["Brodeur"]] <- read_dta("data/Brodeur/merged.dta") %>% 
  # rename(z = myz) %>% 
  # mutate(z = ifelse(abs(z) > 20, 20, z)) %>% 
  # mutate(trunc = ifelse(abs(z) == 20, "truncated", "not truncated")) %>% 
  filter(method=="RCT") %>%
  # mutate(
  # prereg = factor(prereg, labels=c("not pre-registered","pre-registered")), 
  # papregistry = factor(papregistry,labels=c("no analysis plan","analysis plan"))) %>% 
  transmute(
    metaid = NA,
    studyid = title,
    z = myz,
    b = NA,
    se = NA,
    year = year)



# Askarov -----

# data from https://github.com/anthonydouc/Datasharing/blob/master/Stata/Mandatory%20data-sharing%2030%20Aug%202022.dta

dt_list[["Askarov"]] <- read_dta("data/Askarov/Mandatory data-sharing 30 Aug 2022.dta") %>% 
  mutate(z = effectsize/standarderror) %>% 
  dplyr:: filter(Excludegroup==0) %>% 
  transmute(
    metaid = NA,
    studyid = studyid,
    z = effectsize/standarderror,
    b = effectsize,
    se = standarderror,
    year = YEARintervention)
# I'm guessing:
# mutate(data_sharing = (EVENT==0 | INTERVENTION==1)) %>% 
# filter(!is.na(z)) %>% 
# group_by(studyid) %>% 
# mutate(k=n()) %>% 
# ungroup()



# Arel-Bundock -----

# dataset from https://osf.io/fgdet
# requires running make to obtain one of the datasets estimates_rb_vab_mma.csv
# and them get_data.R to replicate the approach of authors to creating a single estimates.csv

# "We examine statistical power in political science by assembling a dataset of 16,649
# hypothesis tests, grouped in 351 meta-analyses, reported in 46 peer-reviewed
# meta-analytic articles"

dt_list[["Arel-Bundock"]] <- read.csv2("data/ArelBundock/estimates.csv",sep=",") %>% 
  transmute(
    metaid = meta_id, 
    studyid = study_id,
    z = as.numeric(estimate)/as.numeric(std.error),
    b = as.numeric(estimate),
    se = as.numeric(std.error),
    year = study_year) 
# filter(!is.na(z)) %>% 
# group_by(study_id) %>% 
# mutate(k=n()) %>% 
# ungroup() 



# What Works Clearinghouse -----

# Erik:
# The data about educational studies were downloaded by Chris Mead from the 
# “What Works Clearinghouse” on December 29, 2023. The website is 
# https://ies.ed.gov/ncee/WWC/StudyFindings 

dt_list[["WWC"]] <- read_csv("data/WWC/Kraft_wwc_merge.csv", show_col_types = FALSE) %>% 
  mutate(pval = as.numeric(f_p_Value_WWC),
         b = as.numeric(Effect.size)) %>% 
  mutate(sign = sign(b),
         z = sign(b)*qnorm(1 - pval/2)) %>% 
  transmute(
    metaid = NA, 
    studyid = cite_trunc,
    z = sign(b)*qnorm(1 - pval/2),
    b = b,
    se = NA,
    year = Publication.year) 

# potentially also of interest:
# read_csv("data/WWC/Kraft_wwc_merge.csv") %>% pull(Academic.subject) %>% table

  # dplyr::filter(!is.na(z)) %>% 
  # group_by(cite_trunc) %>% 
  # mutate(k=n()) %>% 
  # ungroup()



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

dt_list[["Yang et al"]] <- read.csv("data/Yang/dat_processed_rob.csv") %>% 
  transmute(
    metaid = NA, 
    studyid = study,
    z = z,
    b = es,
    se = se) %>%
  mutate(year = sapply(studyid, extract_year_yang))
  # mutate(year = as.numeric(sub(".*?(\\d{4}).*", "\\1", studyid))) %>% summary
  # filter(!is.na(z)) %>%
  # group_by(study) %>% 
  # mutate(k=n())


# Costello and Fox -----

# Data from @costello2022decline which were further processed by @yang2024large 
# and made available at 
# [Github](https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/main/main_dat_processed.csv)

dt_list[["Costello and Fox"]] <- 
  read.csv("data/Yang/main_dat_processed.csv") %>% 
  transmute(
    metaid = meta.analysis.paper, 
    studyid = study2,
    z = z,
    b = eff.size,
    se = se.eff.size,
    year = study.year) 

  # # pull(meta.analysis.id) %>% n_distinct()
  # filter(!is.na(z)) %>% 
  # # group_by(key) %>% 
  # # # Erik chose one 
  # # slice_sample(n = 1) %>% 
  # group_by(study2) %>% 
  # mutate(k = n())



# Jager and Leek -----

load("data/JagerLeek/pvalueData.rda")

dt_list[["Jager and Leek"]] <-  
  pvalueData %>% 
  data.frame() %>% 
  mutate_at(c('pvalue','year'), as.numeric) %>%
  transmute(
    metaid = NA,
    studyid = title,
    z=-qnorm(pvalue/2),
    b = NA, se = NA,
    truncated = factor(pvalueTruncated,labels=c("not truncated","truncated")),
    year = year) 
  # group_by(title) %>%  
  # mutate(k=n()) %>% 
  # ungroup()



# Sladekova -----

dfs <- lapply(list.files("data/Sladekova/", full.names = TRUE), read.csv)
names(dfs) <- list.files("data/Sladekova/")

dt_list[["Sladekova"]] <- lapply(dfs, function(df){
  df %>% 
    filter(!is.na(yi) & !is.na(vi)) %>% 
    transmute(
      b = 0.5*log((1 + yi)/(1 - yi)),
      se = sqrt(vi)) 
      # studyid = author, 
      # year = year)
}) %>% 
  bind_rows(.id = "metaid") %>% 
  filter(!is.na(b) & !is.infinite(b)) %>% 
  mutate(z = b/se,
         year = NA,
         studyid = 1:nrow(.)) %>% 
  filter(!is.na(metaid)) 
  # filter(!is.na(z)) %>%
  # group_by(metaid) %>% 
  # mutate(k=n()) %>% 
  # ungroup() 

# year and studyid could be extracted, but they would need some cleaning up of strings
# in each of the 406 files, so it may be best to do it later



# Metapsy -----

dt_list[["Metapsy"]] <- readRDS("data/Metapsy/metapsy_dt.rds") %>% 
  lapply(function(df) {
    if (all(c("study", "year", ".g", ".g_se") %in% colnames(df))) {
      return(df[, c("study", "year", ".g", ".g_se"), drop = FALSE])
    } else {
      return(data.frame())
    }
  }) %>% 
  bind_rows(.id = "metaid") %>% 
  transmute(
    metaid = metaid,
    studyid = study,
    year = year,
    z = .g/.g_se,
    b = .g,
    se = .g_se)
  # filter(!is.na(z)) %>%
  # group_by(set, study) %>% 
  # mutate(k=n()) %>% 
  # ungroup() 



# Barnett and Wren -----

load("data/BarnettWren/Georgescu.Wren.RData")

dt_list[["BarnettWren"]] <- complete %>% 
  filter(!mistake) %>% 
  # 0.3% of available values have CI widths other than 95%, let's remove these
  # but if ci.level is unknown, assume that it's actually 95% 
  mutate(ci.level = ifelse(is.na(ci.level), 0.95, ci.level)) %>% 
  filter(ci.level == 0.95) %>% 
  mutate(se = (log(upper) - log(lower))/(2*1.96)) %>% 
  mutate(b = (log(upper) + log(lower))/2) %>% 
  mutate(z = b/se) %>% 
  transmute(metaid = NA,
            studyid = pubmed,
            z = b/se,
            b = b,
            se = se,
            year = Year)
# filter(!is.na(z))
# There is median 2 values per paper (max = 5,045), Erik sampled one from each
# (dplyr syntax can be a tiny bit slow here, unfortunately)
# group_by(pubmed) %>%
# slice(sample(n(), 1)) %>%
# ungroup()


# Cochrane -----

load("data/Cochrane/CDSR.RData") #this has object 'data' in it, with >400,000 rows

dt_list[["CDSR"]] <- data %>% 
  filter(RCT=="yes") %>% 
  filter(outcome.group=="efficacy" & outcome.nr==1) %>% 
  transmute(
    metaid = id,
    studyid = study.name,
    z = b/s,
    b = b,
    se = s,
    year = study.year)



# Processing of individual datasets and binding into a single large dataset ----
bear <- dt_list %>% 
  lapply(function(x) {x$studyid <- as.character(x$studyid); x}) %>% 
  lapply(filter, !is.na(z)) %>% 
  bind_rows(.id = "dataset") %>% 
  group_by(dataset, metaid, studyid) %>% 
  mutate(k = n()) %>% 
  ungroup() %>% 
  group_by(dataset, metaid) %>% 
  mutate(j = ifelse(is.na(metaid), NA, n())) %>% 
  ungroup()

saveRDS(bear, "data/BEAR.rds")
