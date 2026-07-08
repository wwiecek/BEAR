# Fit subgroup mixture models for selected comparison categories.
# Run explore.R after this script to compare the saved subgroup fits.

library(tidyverse)
library(tictoc)

source("R/helpers.R")
source("R/mix.R")

set.seed(1990)

fit_file <- "explore/subgroups/fits.rds"
min_rows <- 1000
fit_mode <- "unconstr"

dir.create(dirname(fit_file), showWarnings = FALSE, recursive = TRUE)

safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

make_fit_name <- function(category, dataset, subgroup) {
  paste(safe_name(category), dataset, safe_name(subgroup), sep = "__")
}

prepare_bear <- function() {
  readRDS("BEAR.rds") %>%
    mutate(z_operator = if_else(is.na(z_operator), "=", z_operator)) %>%
    filter(!is.na(z))
}

build_subgroup_manifest <- function(bear) {
  measure_manifest <- bear %>%
    filter(dataset %in% c("Cochrane", "clinicaltrials", "CostelloFox", "Yang"),
           !is.na(measure)) %>%
    count(category = "Measure", dataset, subgroup = measure, name = "N")

  prereg_manifest <- bear %>%
    filter(dataset == "Brodeur", !is.na(subset)) %>%
    count(category = "Pre-registration", dataset, subgroup = subset, name = "N")

  phase_manifest <- bear %>%
    filter(dataset == "clinicaltrials", !is.na(subset)) %>%
    count(category = "Phase", dataset, subgroup = subset, name = "N")

  specialty_manifest <- bear %>%
    filter(dataset == "Cochrane", !is.na(subset)) %>%
    count(category = "Cochrane specialty", dataset, subgroup = subset,
          name = "N")

  bind_rows(measure_manifest, prereg_manifest, phase_manifest,
            specialty_manifest) %>%
    filter(N >= min_rows) %>%
    mutate(
      fit_name = pmap_chr(
        list(category, dataset, subgroup),
        make_fit_name
      ),
      subgroup_variable = case_when(
        category == "Measure" ~ "measure",
        TRUE ~ "subset"
      )
    ) %>%
    arrange(
      factor(category, c("Measure", "Pre-registration", "Phase",
                         "Cochrane specialty")),
      dataset,
      desc(N)
    )
}

subset_df <- function(bear, dataset, subgroup_variable, subgroup) {
  bear %>%
    filter(
      dataset == .env$dataset,
      .data[[subgroup_variable]] == subgroup
    ) %>%
    calc_study_weights() %>%
    thin_df()
}

save_fit_state <- function(fits, dfs, manifest) {
  saveRDS(
    list(
      fits = fits,
      dfs = dfs,
      manifest = manifest,
      min_rows = min_rows,
      fit_mode = fit_mode,
      generated_at = Sys.time()
    ),
    fit_file
  )
}

load_existing_fit_state <- function() {
  if(file.exists(fit_file)) {
    readRDS(fit_file)
  } else {
    list(
      fits = list(),
      dfs = list(),
      manifest = tibble(
        fit_name = character(),
        category = character(),
        dataset = character(),
        subgroup = character(),
        N = integer()
      )
    )
  }
}

bear <- prepare_bear()
subgroup_manifest <- build_subgroup_manifest(bear)

subgroup_dfs <- pmap(
  subgroup_manifest %>% select(dataset, subgroup_variable, subgroup),
  ~ subset_df(bear, ..1, ..2, ..3)
)
subgroup_dfs <- setNames(subgroup_dfs, subgroup_manifest$fit_name)

existing <- load_existing_fit_state()
subgroup_fits <- if(identical(existing$fit_mode, fit_mode)) existing$fits else list()
if(!identical(existing$fit_mode, fit_mode) && length(existing$fits) > 0)
  cat("Dropping saved subgroup fits from previous optimisation mode\n")

old_manifest <- existing$manifest %>%
  select(any_of(c("fit_name", "category", "dataset", "subgroup", "N")))

stale_fits <- old_manifest %>%
  inner_join(
    subgroup_manifest %>%
      select(fit_name, category, dataset, subgroup, N),
    by = "fit_name",
    suffix = c("_old", "_new")
  ) %>%
  filter(category_old != category_new | dataset_old != dataset_new |
           subgroup_old != subgroup_new | N_old != N_new) %>%
  pull(fit_name)

if(length(stale_fits) > 0) {
  cat("Dropping stale subgroup fits:", paste(stale_fits, collapse = ", "), "\n")
  subgroup_fits[stale_fits] <- NULL
}
subgroup_fits <- subgroup_fits[intersect(names(subgroup_fits),
                                         subgroup_manifest$fit_name)]

missing_fits <- setdiff(subgroup_manifest$fit_name, names(subgroup_fits))
cat(length(missing_fits), "subgroup fits to run\n")

for(nm in missing_fits) {
  cat(nm, "\n")
  tic()
  subgroup_fits[[nm]] <- fit_mixture_df(subgroup_dfs[[nm]], mode = fit_mode)
  toc()
  save_fit_state(subgroup_fits, subgroup_dfs, subgroup_manifest)
}

save_fit_state(subgroup_fits, subgroup_dfs, subgroup_manifest)
