# dataset from https://osf.io/fgdet
# requires running make to obtain one of the datasets estimates_rb_vab_mma.csv
# and then this additional script replicate the approach of authors to creating a single estimates.csv

# "We examine statistical power in political science by assembling a dataset of 16,649
# hypothesis tests, grouped in 351 meta-analyses, reported in 46 peer-reviewed
# meta-analytic articles"



# PROCESSING DATA 

# This is the code that Ryan Briggs uses in his project (in prep.R) to create a CSV that merges data from 
# two CSVs, 'briggs' and 'doucouliagos'. I copy that code verbatim but skip extra processing that he does for
# purposes of his study.

library(tidyverse)
# Load and merge the two datasets
briggs <- read_csv("data_raw/ArelBundock/ps_power_replication_20241010/data/estimates_rb_vab_mma.csv") |>
  mutate(sample = "Briggs",
         study_year = ifelse(is.na(study_year),
                             as.numeric(str_extract(study_id, "[0-9]{4}")),
                             study_year))

doucouliagos_meta_id <- data.table::fread(("data_raw/ArelBundock/ps_power_replication_20241010/data/doucouliagos_meta_id.csv"))
doucouliagos <- data.table::fread(("data_raw/ArelBundock/ps_power_replication_20241010/data/estimates_doucouliagos_2021-12-19.csv")) |>
  # type matching with briggs
  transform(study_id = as.character(study_id))

dat <- bind_rows(briggs, doucouliagos) |>
  mutate(z_stat = estimate / std.error,
         dv = ifelse(is.na(dv), "", dv),
         iv = ifelse(is.na(iv), "", iv),
         question_id = ifelse(sample == "Briggs", paste(meta_id, iv, dv, sep = "; "), question_id)) |>
  select(-c(p.value, statistic_t, conf.high, conf.low))


# Drops missing Meta ID.
# Removes these from Chris' data:
# "Get out to vote Direct Mail_With Authors.xlsx", "Get out to vote PHONE with Authors.xlsx", "Get out to vote TEXT_ With Authors.xlsx", "Copy of Get out to vote door to door_With Authors.xlsx", "wage impact of teachers unions.xlsx"
dat <- dat |> filter(!is.na(meta_id))


# Drop problematic observations before computing truth and power 
dat <- dat |>
  filter(!is.na(estimate),
         !is.nan(estimate),
         !is.infinite(estimate),
         !is.na(std.error),
         !is.nan(std.error),
         !is.infinite(std.error),
         # Pure zero standard errors are probably data entry mistakes. This
         # tolerance is arbitrary, but we tested different thresholds and
         # it made no substantive difference. We also checked to see if
         # excluding abs(estimate)<=1e-5 made a difference. It did not.
         std.error > 1e-10) |>
  # mark all rows with n_per_question < 5 for removal.
  # TriWen2020 especially contributed hundred of rows with few obs per question
  group_by(question_id) |>
  add_count() |>
  ungroup() |>
  filter(n >= 5)

# write_csv(dat, ("data/Arel-Bundock/estimates.csv"))
saveRDS(dat, "data/ArelBundock.rds")
