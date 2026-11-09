# Apply recorded manual review decisions to the final ClinicalTrials.gov output.
# Decisions are tracked in process/clinicaltrials.gov/lib/manual_review/.
# Currently: non-inferiority test type and margin. Decisions in
# non_inferiority_review.csv are keyed by the md5 of the description text and
# override the rule-based values from step 07; descriptions the rules cannot
# settle and that have no decision are written to a review queue.

library(dplyr)
library(purrr)
library(readr)
library(stringr)

source("process/clinicaltrials.gov/lib/paths.R")
source("process/clinicaltrials.gov/lib/non_inferiority.R")
source("R/write_summary.R")

out_path <- "data/clinicaltrialsgov.rds"
ni_descriptions_path <- file.path(ctgov_intermediate_dir, "non_inferiority_descriptions.rds")
ni_queue_path <- file.path(ctgov_manual_review_dir, "non_inferiority_review_queue.csv")

public <- readRDS(out_path)
ni_descriptions <- readRDS(ni_descriptions_path)
ni_review <- read_csv(ctgov_ni_review_path,
                      col_types = cols(.default = col_character()), na = character())

stopifnot(
  !anyDuplicated(ni_review$description_id),
  all(ni_review$reviewed_type %in% c(ctgov_ni_types, "FROM_TYPE", "NO_SIGNAL"))
)

# Non-inferiority test type and margin -----

ni_final <- ni_descriptions %>%
  left_join(ni_review %>% select(description_id, reviewed_type, reviewed_margin),
            by = "description_id") %>%
  mutate(
    reviewed = !is.na(reviewed_type),
    final_type = coalesce(reviewed_type, rule_type),
    final_margin = if_else(reviewed, na_if(reviewed_margin, ""), rule_margin)
  )

n_before <- nrow(public)
public <- public %>%
  left_join(ni_final %>% select(non_inferiority_description, final_type, final_margin),
            by = "non_inferiority_description", na_matches = "never") %>%
  mutate(
    author_row = effect_source == "author_reported",
    non_inferior_type_constructed = if_else(
      author_row,
      ctgov_ni_resolve_type(final_type, non_inferiority_type, non_inferior_desc_exists),
      NA_character_),
    non_inferior_margin = if_else(author_row & non_inferior_desc_exists, final_margin, NA_character_)
  ) %>%
  select(-author_row, -final_type, -final_margin)

ni_queue <- ni_final %>%
  filter(!reviewed, !is.na(review_reason)) %>%
  arrange(review_reason, desc(rows)) %>%
  select(description_id, review_reason, rows, aact_types, rule_type, rule_margin,
         non_inferiority_description)

validation_checks <- tibble::tibble(
  check = c(
    "row_count_unchanged",
    "valid_non_inferior_type_constructed",
    "raw_rows_have_no_non_inferiority_columns",
    "margin_only_with_description"
  ),
  passed = c(
    nrow(public) == n_before,
    all(public$non_inferior_type_constructed[public$effect_source == "author_reported"] %in%
          ctgov_ni_types),
    all(is.na(public$non_inferior_type_constructed[public$effect_source == "raw_derived"])),
    all(is.na(public$non_inferior_margin) | public$non_inferior_desc_exists %in% TRUE)
  )
)

print(validation_checks, n = nrow(validation_checks), width = Inf)
stopifnot(all(validation_checks$passed))

save_rds_summary(public, out_path)
write_csv(ni_queue, ni_queue_path, na = "")

message(
  "Non-inferiority review: ", sum(ni_final$reviewed), " of ", nrow(ni_final),
  " descriptions have a recorded decision; ",
  sum(!ni_review$description_id %in% ni_final$description_id),
  " recorded decisions are not used by this snapshot."
)
if (nrow(ni_queue) > 0) {
  message(
    nrow(ni_queue), " descriptions (", sum(ni_queue$rows), " rows) need manual review; ",
    "they keep the rule-based values. Queue: ", ni_queue_path
  )
}
