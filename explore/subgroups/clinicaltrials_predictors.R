# Explore ClinicalTrials.gov predictors of z-value extremity.
# Descriptive summaries and author/raw paired diagnostics run by default;
# mixture fitting is opt-in because it creates bulky cached fits and plots.

library(tidyverse)
library(patchwork)
library(scales)

source("R/helpers.R")
source("R/mix.R")
source("R/plot_mixture.R")
source("R/fit_density_calc.R")
source("R/psr.R")

set.seed(20260619)

out_dir <- "explore/subgroups/output/clinicaltrials_predictors"
fit_file <- file.path(out_dir, "ctgov_predictor_fits.rds")
min_rows_summary <- 500
min_rows_mixture <- 500
max_rows_mixture <- 3000
fit_mode <- "unconstr"
run_mixtures <- FALSE
plot_saved_mixtures <- TRUE
predictor_levels <- c("effect source", "phase", "domain", "trial type",
                      "measure")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sig_cut <- qnorm(.975)

clean_group <- function(x, missing = "unknown") {
  x <- as.character(x)
  x <- if_else(is.na(x) | x == "", missing, x)
  str_to_sentence(str_replace_all(str_to_lower(x), "_", " "))
}

safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

clean_measure <- function(measure_class, raw_measure, scale) {
  clean_group(coalesce(measure_class, raw_measure, scale)) %>%
    recode("Raw" = "Unclassified raw scale")
}

summarise_effects <- function(df) {
  df %>%
    summarise(
      N = n(),
      n_trials = n_distinct(nct_id),
      n_outcomes = n_distinct(outcome_id, na.rm = TRUE),
      prop_significant = mean(significant, na.rm = TRUE),
      median_abs_z = median(abs_z, na.rm = TRUE),
      q75_abs_z = quantile(abs_z, .75, na.rm = TRUE),
      median_abs_z_sig = median(abs_z[significant], na.rm = TRUE),
      prop_author_reported = mean(effect_source == "author_reported"),
      prop_raw_derived = mean(effect_source == "raw_derived"),
      .groups = "drop"
    )
}

add_group <- function(df, predictor, subgroup) {
  tibble(effect_id = df$effect_id, predictor = predictor, subgroup = subgroup) %>%
    filter(!is.na(subgroup), subgroup != "")
}

ct_all <- readRDS("data/clinicaltrialsgov.rds") %>%
  mutate(
    z_operator = replace_na(z_operator, "="),
    abs_z = abs(z),
    significant = z_operator != "<" & abs_z >= sig_cut,
    metaid = "clinicaltrials",
    studyid = nct_id,
    phase_group = clean_group(phase),
    domain = clean_group(domain_primary),
    trial_type = clean_group(intervention_type_primary),
    measure = clean_measure(measure_class, raw_measure, scale),
    source = clean_group(effect_source)
  ) %>%
  filter(!is.na(z))

ct <- ct_all %>%
  filter(include_in_bear, study_type == "INTERVENTIONAL",
         allocation == "RANDOMIZED")

paired_pool <- ct_all %>%
  filter(study_type == "INTERVENTIONAL", allocation == "RANDOMIZED",
         !is.na(author_raw_overlap_key),
         effect_source %in% c("author_reported", "raw_derived"))

sample_flow <- tibble(
  step = c("all rows with z", "included in BEAR",
           "randomized interventional BEAR rows",
           "author/raw overlap rows with z"),
  N = c(nrow(ct_all), sum(ct_all$include_in_bear, na.rm = TRUE),
        nrow(ct), nrow(paired_pool)),
  n_trials = c(n_distinct(ct_all$nct_id),
               n_distinct(ct_all$nct_id[ct_all$include_in_bear]),
               n_distinct(ct$nct_id), n_distinct(paired_pool$nct_id))
)
write_csv(sample_flow, file.path(out_dir, "sample_flow.csv"))

group_rows <- bind_rows(
  add_group(ct, "effect source", ct$source),
  add_group(ct, "phase", ct$phase_group),
  add_group(ct, "domain", ct$domain),
  add_group(ct, "trial type", ct$trial_type),
  add_group(ct, "measure", ct$measure)
)

overall <- summarise_effects(ct)
summary_table <- group_rows %>%
  inner_join(ct_all, by = "effect_id") %>%
  group_by(predictor, subgroup) %>%
  summarise_effects() %>%
  filter(N >= min_rows_summary) %>%
  mutate(
    prop_sig_diff = prop_significant - overall$prop_significant,
    median_abs_z_diff = median_abs_z - overall$median_abs_z,
    score = sqrt(pmin(N, 10000) / 10000) *
      (abs(prop_sig_diff) + .15 * abs(median_abs_z_diff))
  ) %>%
  mutate(predictor = factor(predictor, predictor_levels)) %>%
  arrange(predictor, desc(N)) %>%
  mutate(predictor = as.character(predictor))

mixture_manifest <- summary_table %>%
  filter(N >= min_rows_mixture) %>%
  mutate(fit_name = paste("ctgov", safe_name(predictor),
                          safe_name(subgroup), sep = "__")) %>%
  select(fit_name, predictor, subgroup, N, prop_significant, median_abs_z,
         median_abs_z_sig, score) %>%
  arrange(predictor, desc(score), desc(N))

write_csv(summary_table,
          file.path(out_dir, "ctgov_predictor_summaries.csv"))
write_csv(mixture_manifest, file.path(out_dir, "ctgov_mixture_manifest.csv"))

paired <- paired_pool %>%
  group_by(author_raw_overlap_key, effect_source) %>%
  summarise(
    n_effects = n(),
    z = median(z, na.rm = TRUE),
    abs_z = median(abs_z, na.rm = TRUE),
    significant = mean(significant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = effect_source,
              values_from = c(n_effects, z, abs_z, significant)) %>%
  filter(!is.na(z_author_reported), !is.na(z_raw_derived)) %>%
  left_join(
    paired_pool %>%
      group_by(author_raw_overlap_key) %>%
      summarise(
        nct_id = first(nct_id),
        phase = first(phase_group),
        domain = first(domain),
        trial_type = first(trial_type),
        measure = first(measure),
        .groups = "drop"
      ),
    by = "author_raw_overlap_key"
  ) %>%
  mutate(
    z_diff = z_author_reported - z_raw_derived,
    abs_z_diff = abs_z_author_reported - abs_z_raw_derived,
    same_sign = sign(z_author_reported) == sign(z_raw_derived),
    same_significance = (significant_author_reported >= .5) ==
      (significant_raw_derived >= .5),
    raw_more_extreme = abs_z_raw_derived > abs_z_author_reported
  )

paired_summary <- paired %>%
  summarise(
    N = n(),
    n_trials = n_distinct(nct_id),
    cor_z = cor(z_author_reported, z_raw_derived, use = "complete.obs"),
    cor_abs_z = cor(abs_z_author_reported, abs_z_raw_derived,
                    use = "complete.obs"),
    median_abs_z_author = median(abs_z_author_reported, na.rm = TRUE),
    median_abs_z_raw = median(abs_z_raw_derived, na.rm = TRUE),
    median_abs_z_diff = median(abs_z_diff, na.rm = TRUE),
    prop_raw_more_extreme = mean(raw_more_extreme, na.rm = TRUE),
    prop_same_sign = mean(same_sign, na.rm = TRUE),
    prop_same_significance = mean(same_significance, na.rm = TRUE)
  )

paired_by_measure <- paired %>%
  group_by(measure) %>%
  summarise(
    N = n(),
    n_trials = n_distinct(nct_id),
    median_abs_z_author = median(abs_z_author_reported, na.rm = TRUE),
    median_abs_z_raw = median(abs_z_raw_derived, na.rm = TRUE),
    median_abs_z_diff = median(abs_z_diff, na.rm = TRUE),
    prop_raw_more_extreme = mean(raw_more_extreme, na.rm = TRUE),
    prop_same_significance = mean(same_significance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(N >= min_rows_summary) %>%
  arrange(desc(N))

write_csv(paired, file.path(out_dir, "paired_author_raw_effects.csv"))
write_csv(paired_summary, file.path(out_dir, "paired_author_raw_summary.csv"))
write_csv(paired_by_measure,
          file.path(out_dir, "paired_author_raw_by_measure.csv"))

paired_plot_df <- paired %>%
  mutate(plot_measure = fct_lump_n(measure, n = 7))

z_scatter <- ggplot(paired_plot_df, aes(z_author_reported, z_raw_derived)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55") +
  geom_point(aes(colour = plot_measure), alpha = .25, size = .8) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  labs(x = "Author-reported z", y = "Raw-derived z", colour = "Measure") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

abs_z_scatter <- ggplot(paired_plot_df,
                        aes(abs_z_author_reported, abs_z_raw_derived)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55") +
  geom_point(aes(colour = plot_measure), alpha = .25, size = .8) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(x = "Author-reported |z|", y = "Raw-derived |z|",
       colour = "Measure") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

measure_diff_plot <- paired_by_measure %>%
  mutate(measure = fct_reorder(measure, median_abs_z_diff)) %>%
  ggplot(aes(median_abs_z_diff, measure)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(aes(size = N)) +
  labs(x = "Median author |z| minus raw |z|", y = NULL, size = "Pairs") +
  theme_bw(base_size = 11)

ggsave(file.path(out_dir, "paired_author_raw_z_scatter.pdf"), z_scatter,
       width = 18, height = 14, units = "cm")
ggsave(file.path(out_dir, "paired_author_raw_abs_z_scatter.pdf"),
       abs_z_scatter, width = 18, height = 14, units = "cm")
ggsave(file.path(out_dir, "paired_author_raw_by_measure.pdf"),
       measure_diff_plot, width = 18, height = 12, units = "cm")

make_subset_df <- function(predictor, subgroup) {
  group_rows %>%
    filter(predictor == .env$predictor, subgroup == .env$subgroup) %>%
    inner_join(ct_all, by = "effect_id") %>%
    calc_study_weights() %>%
    thin_df(N = max_rows_mixture, seed = 20260619)
}

summarise_fit <- function(fit, df, predictor, subgroup) {
  psr <- powsignrep(fit)
  psr_sig <- powsignrep(fit, z_star = sig_cut)
  tibble(
    predictor = predictor,
    subgroup = subgroup,
    N = nrow(df),
    omega = fit$omega[1],
    power = mean(psr$power),
    sign = mean(psr$sgn),
    replication = mean(psr$rep),
    replication_sig = mean(psr_sig$rep)
  )
}

save_fit_state <- function(fits, dfs, manifest) {
  saveRDS(list(fits = fits, dfs = dfs, manifest = manifest,
               min_rows_mixture = min_rows_mixture,
               max_rows_mixture = max_rows_mixture,
               fit_mode = fit_mode, generated_at = Sys.time()),
          fit_file)
}

if(run_mixtures || (plot_saved_mixtures && file.exists(fit_file))) {
  old <- if(file.exists(fit_file)) readRDS(fit_file) else NULL
  old_compatible <- !is.null(old) && identical(old$fit_mode, fit_mode) &&
    identical(old$min_rows_mixture, min_rows_mixture) &&
    identical(old$max_rows_mixture, max_rows_mixture)
  subgroup_fits <- if(old_compatible)
    old$fits else list()
  subgroup_fits <- subgroup_fits[intersect(names(subgroup_fits),
                                           mixture_manifest$fit_name)]

  if(run_mixtures) {
    subgroup_dfs <- pmap(mixture_manifest %>% select(predictor, subgroup),
                         make_subset_df)
    subgroup_dfs <- setNames(subgroup_dfs, mixture_manifest$fit_name)
  } else {
    subgroup_dfs <- old$dfs[intersect(names(old$dfs), names(subgroup_fits))]
  }

  for(nm in setdiff(mixture_manifest$fit_name, names(subgroup_fits))) {
    if(!run_mixtures) next
    cat("Fitting", nm, "\n")
    subgroup_fits[[nm]] <- fit_mixture_df(subgroup_dfs[[nm]],
                                          mode = fit_mode)
    save_fit_state(subgroup_fits, subgroup_dfs, mixture_manifest)
  }
  save_fit_state(subgroup_fits, subgroup_dfs, mixture_manifest)

  plot_manifest <- mixture_manifest %>%
    filter(fit_name %in% names(subgroup_fits),
           fit_name %in% names(subgroup_dfs))

  if(nrow(plot_manifest) > 0) {
    fit_table <- pmap_dfr(
      plot_manifest %>% select(fit_name, predictor, subgroup),
      ~ summarise_fit(subgroup_fits[[..1]], subgroup_dfs[[..1]], ..2, ..3)
    ) %>%
      mutate(across(c(omega, power, sign, replication, replication_sig),
                    ~ round(.x, 3))) %>%
      arrange(predictor, desc(N))
    write_csv(fit_table, file.path(out_dir, "ctgov_mixture_fit_summary.csv"))

    mixture_plots <- pmap(
      plot_manifest %>% select(fit_name, predictor, subgroup),
      ~ plot_mixture_v4(
        subgroup_fits[[..1]],
        subgroup_dfs[[..1]] %>% mutate(group = ..2),
        nm = paste(..2, ..3, sep = ": "),
        col = "#377EB8",
        ymax = .6,
        show_corrected = TRUE,
        align_corrected_above_threshold = TRUE
      )
    )
    ggsave(file.path(out_dir, "ctgov_predictor_mixtures.pdf"),
           wrap_plots(mixture_plots, ncol = 5),
           width = 35, height = max(10, 5 * ceiling(length(mixture_plots) / 5)),
           units = "cm", limitsize = FALSE)

    omega_plot <- fit_table %>%
      mutate(omega_plot = pmin(pmax(omega, 0), 1),
             label = paste(predictor, subgroup, sep = ": ")) %>%
      ggplot(aes(omega_plot, replication_sig, colour = predictor)) +
      geom_point(size = 2.2) +
      ggrepel::geom_text_repel(aes(label = label), size = 2.5,
                               max.overlaps = Inf, seed = 20260619) +
      scale_x_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, 1)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = expression(omega),
           y = "Pr(replication if |z| > 1.96)", colour = NULL) +
      theme_bw(base_size = 11) +
      theme(legend.position = "bottom")
    ggsave(file.path(out_dir, "ctgov_omega_vs_repl.pdf"), omega_plot,
           width = 22, height = 16, units = "cm")
  }
}
