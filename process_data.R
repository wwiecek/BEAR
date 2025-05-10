# Prepare data -----
library(haven)
library(tidyverse)

dt_list <- list()

# We will want to make use of the following columns:
# study
# year
# effect size
# SE
# z
# metaid
# truncated

# Brodeur et al ------

dt_list[["Brodeur"]] <- read_dta("data/Brodeur/merged.dta") %>% 
  rename(z = myz) %>% 
  mutate(z = ifelse(abs(z) > 20, 20, z)) %>% 
  mutate(trunc = ifelse(abs(z) == 20, "truncated", "not truncated")) %>% 
  filter(method=="RCT") %>%
  mutate(
    prereg = factor(prereg, labels=c("not pre-registered","pre-registered")), 
    papregistry = factor(papregistry,labels=c("no analysis plan","analysis plan"))) %>% 
  filter(!is.na(z)) %>% 
  group_by(title) %>% 
  mutate(k=n()) %>% 
  ungroup()



# Askarov -----

# data from https://github.com/anthonydouc/Datasharing/blob/master/Stata/Mandatory%20data-sharing%2030%20Aug%202022.dta

dt_list[["Askarov"]] <- read_dta("data/Askarov/Mandatory data-sharing 30 Aug 2022.dta") %>% 
  mutate(z = effectsize/standarderror) %>% 
  dplyr:: filter(Excludegroup==0) %>% 
  # I'm guessing:
  mutate(data_sharing = (EVENT==0 | INTERVENTION==1)) %>% 
  filter(!is.na(z)) %>% 
  group_by(studyid) %>% 
  mutate(k=n()) %>% 
  ungroup()



# Arel-Bundock -----

# dataset from https://osf.io/fgdet
# requires running make to obtain one of the datasets estimates_rb_vab_mma.csv
# and them get_data.R to replicate the approach of authors to creating a single estimates.csv

# "We examine statistical power in political science by assembling a dataset of 16,649
# hypothesis tests, grouped in 351 meta-analyses, reported in 46 peer-reviewed
# meta-analytic articles"

dt_list[["Arel-Bundock"]] <- read.csv2("data/ArelBundock/estimates.csv",sep=",") %>% 
  mutate(z = as.numeric(estimate)/as.numeric(std.error)) %>% 
  filter(!is.na(z)) %>% 
  group_by(study_id) %>% 
  mutate(k=n()) %>% 
  ungroup() 

# Joined and cleaned up list of datasets
dt_clean <- dt_list %>% lapply(ungroup) %>% lapply(select, z, k) %>% bind_rows(.id = "metastudy")



# What Works Clearinghouse -----

# Erik:
# The data about educational studies were downloaded by Chris Mead from the 
# “What Works Clearinghouse” on December 29, 2023. The website is 
# https://ies.ed.gov/ncee/WWC/StudyFindings 

dt_list[["WWC"]] <- read_csv("data/WWC/Kraft_wwc_merge.csv") %>% 
  mutate(pval = as.numeric(f_p_Value_WWC),
         b = as.numeric(Effect.size)) %>% 
  mutate(sign = sign(b),
         z = sign(b)*qnorm(1 - pval/2)) %>% 
  dplyr::filter(!is.na(z)) %>% 
  group_by(cite_trunc) %>% 
  mutate(k=n()) %>% 
  ungroup()



# Yang et al -----

# Datafile dat_processed_rob.csv may be downloaded from 
# https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/sensitivity/dat_processed_rob.csv


dt_list[["Yang et al"]] <- read.csv("data/Yang/dat_processed_rob.csv") %>% 
  filter(!is.na(z)) %>%
  group_by(study) %>% 
  mutate(k=n())


# Costello and Fox -----

# Data from @costello2022decline which were further processed by @yang2024large 
# and made available at 
# [Github](https://github.com/Yefeng0920/replication_EcoEvo_git/blob/main/data/main/main_dat_processed.csv)

dt_list[["Costello and Fox"]] <- 
  read.csv("data/Yang/main_dat_processed.csv") %>% 
  # pull(meta.analysis.id) %>% n_distinct()
  filter(!is.na(z)) %>% 
  group_by(key) %>% 
  # Erik chose one 
  slice_sample(n = 1) %>% 
  group_by(study2) %>% 
  mutate(k = n())



# Jager and Leek -----

load("data/JagerLeek/pvalueData.rda")

dt_list[["Jager and Leek"]] <-  
  pvalueData %>% 
  data.frame() %>% 
  mutate_at(c('pvalue','year'), as.numeric) %>%
  mutate(z=-qnorm(pvalue/2)) %>% 
  mutate(truncated = factor(pvalueTruncated,labels=c("not truncated","truncated"))) %>% 
  group_by(title) %>%  
  mutate(k=n()) %>% 
  ungroup()


# Sladekova -----

dfs <- lapply(list.files("data/Sladekova/", full.names = TRUE), read.csv)
names(dfs) <- list.files("data/Sladekova/")

dt_list[["Sladekova"]] <- lapply(dfs, function(df){
  df = na.omit(df[,c("yi", "vi")])
  df = data.frame(
    b = 0.5*log((1+df$yi)/(1 - df$yi)),
    se = sqrt(df$vi)
  )
  df = df[!is.na(df$b) & !is.infinite(df$b),]
  return(df)
}) %>% 
  bind_rows(.id = "metaid") %>% 
  mutate(z = b/se) %>% 
  filter(!is.na(metaid)) %>%
  filter(!is.na(z)) %>%
  group_by(metaid) %>% 
  mutate(k=n()) %>% 
  ungroup() 



# metaPsy -----

dt_list[["metaPsy"]] <- readRDS("data/metaPsy/metapsy_dt.rds") %>% 
  lapply(function(df) {
    if (all(c("study", ".g", ".g_se") %in% colnames(df))) {
      return(df[, c("study", ".g", ".g_se"), drop = FALSE])
    } else {
      return(data.frame()) 
    }
  }) %>% 
  bind_rows(.id = "set") %>% 
  mutate(z = .g/.g_se) %>% 
  filter(!is.na(z)) %>%
  group_by(set, study) %>% 
  mutate(k=n()) %>% 
  ungroup() 



# Barnett and Wren -----

load("data/BarnettWren/Georgescu.Wren.RData")

dt_list[["BarnettWren"]] <- complete %>% 
  filter(!mistake) %>% 
  mutate(se = (log(upper) - log(lower))/(2*1.96)) %>% 
  mutate(b = (log(upper) + log(lower))/2) %>% 
  mutate(z = b/se) %>% 
  filter(!is.na(z)) %>% 
  # There is median 2 values per paper (max = 5,045), Erik sampled one from each
  # (dplyr syntax can be a tiny bit slow here, unfortunately)
  group_by(pubmed) %>%
  slice(sample(n(), 1)) %>%
  ungroup()



saveRDS(dt_list, file = "data/dt_list.rds") #20-30 MBs of data
