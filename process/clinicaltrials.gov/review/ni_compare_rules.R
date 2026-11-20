# Compare the rule-based non-inferiority type and margin before and after a
# change to lib/non_inferiority.R. See non_inferiority_review_procedure.md.
#
# Run from the BEAR root:
#   1. Rscript process/clinicaltrials.gov/review/ni_compare_rules.R --save
#   2. Change the rules and rerun step 07
#      (process/clinicaltrials.gov/process/07_build_author_reported_candidates.R)
#   3. Rscript process/clinicaltrials.gov/review/ni_compare_rules.R [max_chars]
# Step 3 lists every description whose rule type or margin is new, lost or
# changed, and writes the list to non_inferiority_rule_changes.csv. Read each
# against its text. Descriptions with a recorded decision are listed too but
# keep their decision.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# Same locations as lib/paths.R
descriptions_path <- "data_raw/clinicaltrials.gov/derived/intermediate/non_inferiority_descriptions.rds"
review_dir <- "data_raw/clinicaltrials.gov/validation/manual_review"
before_path <- file.path(review_dir, "non_inferiority_descriptions_before.rds")
changes_path <- file.path(review_dir, "non_inferiority_rule_changes.csv")
decisions_path <- "process/clinicaltrials.gov/lib/manual_review/non_inferiority_review.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && args[1] == "--save") {
  file.copy(descriptions_path, before_path, overwrite = TRUE)
  message("Saved the current rule output to ", before_path)
  quit(save = "no")
}
max_chars <- if (length(args) >= 1) as.integer(args[1]) else 500L
stopifnot("Run with --save before changing the rules" = file.exists(before_path))

before <- readRDS(before_path) %>%
  select(description_id, old_type = rule_type, old_margin = rule_margin)
after <- readRDS(descriptions_path)
decided <- read_csv(decisions_path, col_types = cols(.default = col_character()),
                    na = character())$description_id

changes <- after %>%
  full_join(before, by = "description_id") %>%
  filter(coalesce(rule_type, "") != coalesce(old_type, "") |
           coalesce(rule_margin, "") != coalesce(old_margin, "")) %>%
  mutate(
    kind = case_when(
      is.na(rule_type) ~ "description gone",
      is.na(old_margin) & !is.na(rule_margin) ~ "new margin",
      !is.na(old_margin) & is.na(rule_margin) ~ "lost margin",
      coalesce(rule_margin, "") != coalesce(old_margin, "") ~ "changed margin",
      TRUE ~ "changed type"
    ),
    has_decision = description_id %in% decided
  ) %>%
  arrange(kind, desc(rows)) %>%
  select(kind, has_decision, description_id, rows, old_type, rule_type, old_margin,
         rule_margin, non_inferiority_description)

write_csv(changes, changes_path, na = "")
cat(nrow(changes), "descriptions changed;", sum(changes$has_decision), "have a recorded decision\n")
print(count(changes, kind))
cat("\n")
for (i in seq_len(nrow(changes))) {
  x <- changes[i, ]
  cat(sprintf("[%s] %s%s | type %s -> %s | margin %s -> %s\n", str_sub(x$description_id, 1, 8),
              x$kind, if (x$has_decision) " (decided)" else "", x$old_type, x$rule_type,
              coalesce(x$old_margin, "-"), coalesce(x$rule_margin, "-")))
  cat("  ", str_sub(str_squish(coalesce(x$non_inferiority_description, "")), 1, max_chars), "\n\n",
      sep = "")
}
