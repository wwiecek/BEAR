# Record non-inferiority review decisions in
# lib/manual_review/non_inferiority_review.csv. See
# non_inferiority_review_procedure.md.
#
# Run from the BEAR root:
#   Rscript process/clinicaltrials.gov/review/ni_record_decisions.R decisions.tsv
#
# decisions.tsv has no header and four tab-separated columns:
#   target   a description id, an id prefix of at least 8 characters, or a
#            group such as g12 from the last ni_print_queue.R run
#   type     SUPERIORITY, NON_INFERIORITY, EQUIVALENCE,
#            NON_INFERIORITY_OR_EQUIVALENCE, UNKNOWN, FROM_TYPE, NO_SIGNAL,
#            or the short codes S, NI, EQ, NE, U, T, N
#   margin   margin as text, empty for none
#   note     reason for the decision
# A decision replaces any earlier decision for the same description.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# Same locations as lib/paths.R
decisions_path <- "process/clinicaltrials.gov/lib/manual_review/non_inferiority_review.csv"
descriptions_path <- "data_raw/clinicaltrials.gov/derived/intermediate/non_inferiority_descriptions.rds"
groups_path <- "data_raw/clinicaltrials.gov/validation/manual_review/non_inferiority_review_groups.csv"

type_codes <- c(S = "SUPERIORITY", NI = "NON_INFERIORITY", EQ = "EQUIVALENCE",
                NE = "NON_INFERIORITY_OR_EQUIVALENCE", U = "UNKNOWN",
                T = "FROM_TYPE", N = "NO_SIGNAL")

args <- commandArgs(trailingOnly = TRUE)
stopifnot("Give the path to a decisions TSV" = length(args) == 1)

input <- read_tsv(args[1], col_names = c("target", "type", "reviewed_margin", "note"),
                  col_types = cols(.default = col_character()), na = character(),
                  trim_ws = TRUE)
descriptions <- readRDS(descriptions_path) %>%
  select(description_id, non_inferiority_description)
decisions <- read_csv(decisions_path, col_types = cols(.default = col_character()),
                      na = character())
groups <- if (file.exists(groups_path)) {
  read_csv(groups_path, col_types = cols(.default = col_character()))
} else {
  tibble(group = character(), description_id = character())
}

# Expand groups and id prefixes to full description ids
known_ids <- union(descriptions$description_id, decisions$description_id)
expand_target <- function(target) {
  if (str_detect(target, "^g\\d+$")) {
    ids <- groups$description_id[groups$group == str_remove(target, "^g")]
    if (length(ids) == 0) stop("Unknown group: ", target, call. = FALSE)
    return(ids)
  }
  if (nchar(target) < 8) stop("Id prefix shorter than 8 characters: ", target, call. = FALSE)
  ids <- known_ids[startsWith(known_ids, target)]
  if (length(ids) != 1) stop("Id prefix matches ", length(ids), " descriptions: ", target,
                             call. = FALSE)
  ids
}

new <- input %>%
  mutate(
    description_id = lapply(target, expand_target),
    reviewed_type = if_else(type %in% names(type_codes), type_codes[type], type)
  ) %>%
  unnest(description_id) %>%
  select(description_id, reviewed_type, reviewed_margin, note)

bad_types <- setdiff(new$reviewed_type, type_codes)
if (length(bad_types) > 0) stop("Unknown type: ", paste(bad_types, collapse = ", "), call. = FALSE)
if (anyDuplicated(new$description_id)) stop("A description is decided twice in ", args[1],
                                            call. = FALSE)

# Keep the description text so the file stays readable and can be re-keyed
new <- new %>%
  left_join(descriptions, by = "description_id") %>%
  left_join(decisions %>% select(description_id, old_text = non_inferiority_description),
            by = "description_id") %>%
  mutate(non_inferiority_description = coalesce(non_inferiority_description, old_text)) %>%
  select(-old_text)

out <- decisions %>%
  filter(!description_id %in% new$description_id) %>%
  bind_rows(new) %>%
  arrange(description_id)
write_csv(out, decisions_path, na = "")

message("Recorded ", nrow(new), " decisions (", sum(new$description_id %in% decisions$description_id),
        " replaced earlier ones); ", nrow(out), " decisions in total. Rerun step 10 to apply them.")
