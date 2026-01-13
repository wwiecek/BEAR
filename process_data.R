# Prepare data -----
library(haven)
library(tidyverse)
library(metafor) #for CDSR calculations of effect sizes (binary data)
library(stringi) #for cleaning up some study names

dtlist <- list()

# This mini function tries to deal with very, very small p values and 
# also performs a bit of truncation by default
z_from_p <- function(p, truncate = 100) {
  z <- qnorm(1 - p/2)
  # extra precision recommended by chatGPT
  small_p <- !is.na(p) & p < 1e-15
  z[small_p] <- sqrt(-2 * log(p[small_p]/2))
  z[!is.na(p) & p <= 0] <- truncate
  z[!is.na(p) & p >= 1] <- 0
  z
}

# Brodeur et al ------

dtlist[["Brodeur"]] <- readRDS("data/Brodeur.rds") %>% 
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

dtlist[["Askarov"]] <- readRDS("data/Askarov.rds") %>% 
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

dtlist[["ArelBundock"]] <- 
  # read_csv("data/ArelBundock/estimates.csv", 
  #          show_col_types = FALSE) %>% 
  readRDS("data/ArelBundock.rds") %>% 
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

# using data from WWC website directly
dtlist[["WWC"]] <- readRDS("data/WWC.rds") %>%
  filter(method %in% c("RCT", "quasi")) %>% 
  transmute(
    metaid = NA,
    studyid = study_id,
    method = method,
    measure = NA,
    group = Outcome_Domain,         
    z = sign(b) * z_from_p(pval),
    z_operator = ifelse(pval == 1e-16, ">", "="),
    b = b,
    se = NA_real_,
    ss = ss,
    year = year
  )



# Yang et al -----

dtlist[["Yang"]] <- readRDS("data/Yang.rds") %>% 
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

dtlist[["CostelloFox"]] <- 
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



# Jager and Leek -----

dtlist[["JagerLeek"]] <-  
  readRDS("data/JagerLeek.rds") %>%
  mutate(method = ifelse(grepl("randomized|randomised|controlled", title), "RCT", NA)) %>% 
  transmute(
    metaid = NA,
    studyid = pubmedID,
    method = method,
    measure = NA,
    p = pvalue,
    z = z_from_p(pvalue),
    b = NA, se = NA, ss=NA,
    # about 1/3 truncated, almost always .0001, .001, .01, or .05, so it's "p <"
    z_operator = case_when(
      pvalue == 0 ~ ">", 
      pvalueTruncated == "1" ~ ">", 
      TRUE ~ "="),
    year = year) 



# Sladekova -----

dtlist[["Sladekova"]] <- 
  readRDS("data/Sladekova.rds") %>% 
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

# sort(table(unlist(lapply(readRDS("data/Metapsy.rds"), function(f) names(f)))), decreasing = TRUE)

dtlist[["Metapsy"]] <- readRDS("data/Metapsy.rds") %>% 
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

dtlist[["BarnettWren"]] <- readRDS("data/BarnettWren.rds") %>% 
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



# Cochrane 2025 -----

cdsr_filtered <- readRDS("data/Cochrane.rds") %>% 
  # Remove studies with sample size of zero 
  filter(total1 > 0, total2 > 0, 
         !is.na(measure_group),
         # Even though we make our own calculation, it's better to 
         # Remove small minority of studies that would require more complicated calculations
         # small minority of data is IPD or IV ("results with effects and standard errors 
         # but without the data necessary for their computation") and we exclude these
         outcome.flag %in% c("CONT","DICH"),
         # First comparison and first outcome is most likely to be the primary outcome of interest in a given review
         # THIS REMOVES 95% OF DATA! YOu may want to construct it differently for future analyses
         comparison.nr == 1, outcome.nr == 1)

dtlist[["Cochrane"]] <- rbind(
  dplyr::filter(cdsr_filtered, outcome.flag=="CONT") %>% 
    escalc(m1i=mean1,sd1i=sd1,n1i=total1,
           m2i=mean2,sd2i=sd2,n2i=total2,measure="SMD",
           data=., append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "SMD"),
  dplyr::filter(cdsr_filtered, outcome.flag=="DICH") %>% 
    escalc(ai=events1,n1i=total1,
           ci=events2,n2i=total2,measure="PBIT",
           data=.,append=TRUE) %>% 
    as_tibble() %>% mutate(measure = "probit")
) %>% 
  transmute(
    metaid = id,
    studyid = study.name,
    year = study.year,
    # Still working on this one:
    method = ifelse(rct, "RCT", "unknown"),
    measure = measure,
    z = yi/sqrt(vi),
    b = yi,
    se = sqrt(vi),
    group = as.character(specialty),
    ss = total1 + total2) %>% 
  filter(!is.na(b))

rm(cdsr_filtered)



# Cochrane 2019 -----

# load("data/Cochrane2019/CDSR.RData")  # reads in dataframe "data"
# data_filtered <- data %>%
#   select(-char.interventions) %>%
#   filter(
#     # non-RCT data comprise more than half of all data
#     # RCT=="yes",
#     # small minority of data is IPD or IV ("results with effects and standard errors
#     # but without the data necessary for their computation") and we exclude these
#     outcome.flag %in% c("CONT","DICH"),
#     outcome.group=="efficacy",
#     # This will pare it down from 410,000 rows to 31,000:
#     outcome.nr==1,
#     comparison.nr==1)
# 
# dtlist[["Cochrane2019"]] <- rbind(
#   dplyr::filter(data_filtered, outcome.flag=="CONT") %>%
#     escalc(m1i=mean1,sd1i=sd1,n1i=total1,
#            m2i=mean2,sd2i=sd2,n2i=total2,measure="SMD",
#            data=., append=TRUE) %>%
#     as_tibble() %>% mutate(measure = "SMD"),
#   dplyr::filter(data_filtered, outcome.flag=="DICH") %>%
#     escalc(ai=events1,n1i=total1,
#            ci=events2,n2i=total2,measure="PBIT",
#            data=.,append=TRUE) %>%
#     as_tibble() %>% mutate(measure = "probit")
# ) %>%
#   transmute(
#     metaid = id,
#     studyid = study.name,
#     method = ifelse(RCT == "yes", "RCT", "observational"),
#     measure = measure,
#     z = yi/sqrt(vi),
#     b = yi,
#     se = sqrt(vi),
#     year = study.year,
#     group = as.character(specialty),
#     ss = total1 + total2)
# 
# rm(data, data_filtered)



# EUCTR -----

dtlist[["euctr"]] <- readRDS("data/euctr.rds") %>%
  filter(collection == "EUCTR") %>%
  transmute(
    metaid  = NA,
    studyid = id,
    year,
    measure = measure_class,
    method  = NA_character_,
    z, z_operator,
    b, se,
    ss      = n
  )



# clinicaltrials.gov -----

# dtlist[["Adda"]] <- read_dta("data/Adda/data_counterfactual_analysis.dta") %>% 
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

dtlist[["clinicaltrials"]] <- readRDS("data/clinicaltrialsgov.rds") %>% 
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

dtlist[["Head"]] <- readRDS("data/Head.rds") %>% 
  transmute(metaid = NA,
            studyid = pmid,
            source = section,
            method = NA,
            measure = NA,
            year = year,
            p = p.value,
            z = z_from_p(p.value),
            b = NA,
            se = NA,
            ss = NA,
            # we do not differntiate between leq and lesser, because it doesn't 
            # really change analytical procedures we have in mind
            z_operator = case_when(
              p.value == 0 ~ ">",
              operator == "=" ~ "=",
              operator == "<" ~ ">",
              operator == ">" ~ "<",
              operator == "≤" ~ ">",
              operator == "≥" ~ "<"
            )) 



# Chavalarias -----

dtlist[["Chavalarias"]] <- readRDS("data/Chavalarias.rds") %>% 
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
            z = z_from_p(p),
            b = NA,
            se = NA,
            ss = NA,
            z_operator = case_when(
              p == 0 ~ ">",
              operator == "=" ~ "=",
              operator == "<" ~ ">",
              operator == ">" ~ "<"
            )) 



# Open Science Collab replication project -----

dtlist[["OSC"]] <- readRDS("data/OSC.rds") %>% 
  rename(p_value = T_pval_USE..R.) %>% 
  transmute(
    metaid = NA,
    studyid = Study.Num,
    method = "RCT",
    measure = NA,
    z = z_from_p(p_value),
    z_operator = ifelse(p_value > 0, "=", ">"),
    b = NA,
    se = NA,
    year = NA,
    group = NA,
    ss = as.numeric(N..R.)
  ) %>% as_tibble()



# Bartos -----

# Very nicely organised dataset on exercise which requires no extra work from us
dtlist[["Bartos"]] <- readRDS("data/Bartos.rds") %>% 
  transmute(
    metaid = as.character(meta_id),
    studyid = as.character(id), #these are unique (=same number of IDs as rows in the dataset)
    method = NA,
    measure = effect_size_type,
    z = effect_size/standard_error,
    z_operator = "=",
    b = effect_size,
    se = standard_error,
    year = year,
    group = category, # why not!
    # There is also total m-a size: samples_size; ignoring
    ss = sample_size
  ) 



# psymetadata -----

dtlist[["psymetadata"]] <- readRDS("data/psymetadata.rds")



# Bogdan -----

# x <- read_csv("data/Bogdan/df_combined_pruned_Jan21.csv")
# x <- read_csv("data/Bogdan/df_combined_w_no_sig_all_aff_Jan21.csv")
# x <- read_csv("data/Bogdan/Youyou_etal_replications.csv")
# View(x)
# 
# dtlist[["Bogdan"]] <- x



# Processing of individual datasets and binding into a single large dataset ----
bear <- dtlist %>% 
  lapply(function(x) {x$studyid <- as.character(x$studyid); x}) %>% 
  lapply(filter, !is.na(z)) %>% 
  bind_rows(.id = "dataset")

saveRDS(bear, "data/BEAR.rds")

# Refresh README
rmarkdown::render("README.Rmd", output_format = "github_document")
