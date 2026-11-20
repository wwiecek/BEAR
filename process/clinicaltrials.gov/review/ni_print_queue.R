# Print the non-inferiority review queue written by step 10, grouped into
# near-duplicate descriptions, and save the grouping for
# ni_record_decisions.R. See non_inferiority_review_procedure.md.
#
# Run from the BEAR root:
#   Rscript process/clinicaltrials.gov/review/ni_print_queue.R [from] [to] [max_chars]
# from/to select groups (default all); max_chars caps the printed text of the
# first description in each group (default 600).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# Same locations as lib/paths.R
review_dir <- "data_raw/clinicaltrials.gov/validation/manual_review"
queue_path <- file.path(review_dir, "non_inferiority_review_queue.csv")
groups_path <- file.path(review_dir, "non_inferiority_review_groups.csv")

args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 1L
to <- if (length(args) >= 2) as.integer(args[2]) else Inf
max_chars <- if (length(args) >= 3) as.integer(args[3]) else 600L

queue <- read_csv(queue_path, col_types = cols(.default = col_character(), rows = col_integer()),
                  na = c("", "NA"))
if (nrow(queue) == 0) {
  message("The review queue is empty.")
  quit(save = "no")
}

# Groups share their first 70 letters, ignoring case, digits and punctuation;
# read one member in full and check that the others say the same
queue <- queue %>%
  mutate(
    text = str_squish(str_replace_all(non_inferiority_description, "\\\\([<>\\[\\]&_*])", "\\1")),
    key = str_sub(str_squish(str_remove_all(str_to_lower(text), "[^a-z ]")), 1, 70)
  )
groups <- queue %>%
  group_by(key) %>%
  summarise(
    descriptions = n(), rows = sum(rows),
    reasons = paste(sort(unique(review_reason)), collapse = "; "),
    aact_types = paste(sort(unique(unlist(str_split(aact_types, ";")))), collapse = ";"),
    rule_types = paste(sort(unique(rule_type)), collapse = ";"),
    rule_margins = paste(sort(unique(na.omit(rule_margin))), collapse = "; "),
    ids = paste(str_sub(description_id, 1, 8), collapse = ","),
    text = first(text),
    .groups = "drop"
  ) %>%
  arrange(desc(rows), key) %>%
  mutate(group = row_number())

write_csv(queue %>% select(description_id, key) %>%
            left_join(groups %>% select(key, group), by = "key") %>%
            select(group, description_id),
          groups_path)

cat(nrow(queue), "descriptions in", nrow(groups), "groups;", sum(queue$rows), "rows\n\n")
for (i in which(groups$group >= from & groups$group <= to)) {
  g <- groups[i, ]
  cat(sprintf("g%d | %d descriptions, %d rows | %s\n", g$group, g$descriptions, g$rows, g$reasons))
  cat(sprintf("  types: %s | rule: %s | margin: %s\n", g$aact_types, g$rule_types,
              if (nzchar(g$rule_margins)) g$rule_margins else "-"))
  cat(sprintf("  ids: %s\n", g$ids))
  cat("  ", str_sub(g$text, 1, max_chars), "\n\n", sep = "")
}
