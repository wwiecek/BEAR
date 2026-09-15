# Validate the adjudicated Lang corrections against the pre-review snapshot.
# This checks saved data only; it does not run network lookups or rewrite data.

library(tidyverse)

before <- readRDS("data_raw/Lang/derived/doi_review_20260910/Lang_before.rds")
after <- readRDS("data/Lang.rds")
unchanged <- setdiff(names(before), c("studyid", "source_title", "doi"))
stopifnot(identical(before[unchanged], after[unchanged]),
          identical(before$source_title, after$source_title_original),
          nrow(after) == 3885L, n_distinct(after$paper_id) == 736L,
          n_distinct(after$studyid) == 730L,
          n_distinct(after$studyid[after$lang_main_sample]) == 660L)

expected_ids <- before$studyid
expected_ids[before$source_unique_paperid %in% 2:5] <- "Lang_paper_2"
expected_ids[before$source_unique_paperid %in% 29:31] <- "Lang_paper_29"
expected_ids[before$source_unique_paperid %in% 42:43] <- "Lang_paper_42"
stopifnot(identical(after$studyid, expected_ids))
title_changes <- which(before$source_title != after$source_title)
stopifnot(identical(title_changes, c(327:330, 807:808, 2052L, 2054L)))
stopifnot(all(after$studyid[c(2052L, 2054L)] == "Lang_paper_474"),
          sum(after$studyid == "Lang_paper_474") == 14L,
          sum(after$studyid == "Lang_paper_472") == 6L)

expected <- c(
  Lang_paper_2 = "10.1257/aer.20191586", Lang_paper_29 = "10.1093/qje/qjab016",
  Lang_paper_42 = "10.1257/aer.20201238", Lang_paper_100 = "10.1093/qje/qjab004",
  Lang_paper_223 = "10.1016/j.jdeveco.2015.04.002",
  Lang_paper_324 = "10.1016/j.jpubeco.2015.02.009",
  Lang_paper_472 = "10.1111/ecoj.12505", Lang_paper_474 = "10.1111/ecoj.12448",
  Lang_paper_530 = "10.1016/j.jdeveco.2018.07.008",
  Lang_paper_689 = "10.1093/qje/qjx040")
for (id in names(expected)) {
  stopifnot(all(after$doi[after$studyid == id] == expected[[id]]))
}
articles <- after %>% distinct(studyid, doi)
stopifnot(nrow(articles) == 730L, !anyNA(articles$doi),
          !anyDuplicated(str_to_lower(str_trim(articles$doi))))

audit <- read_csv("data_raw/Lang/derived/lang_doi_journal_lookup.csv",
                  show_col_types = FALSE) %>%
  select(paper_id, source_doi_pattern, legacy_prefix_mismatch)
flagged <- after %>% distinct(paper_id, doi) %>%
  inner_join(filter(audit, legacy_prefix_mismatch), by = "paper_id")
stopifnot(nrow(flagged) == 38L,
          all(str_detect(flagged$doi, flagged$source_doi_pattern)))
retained <- paste0("Lang_paper_", c(208, 279, 280, 570, 581, 584:586,
                                  589:591, 627, 641, 656))
stopifnot(identical(before$doi[before$paper_id %in% retained],
                    after$doi[after$paper_id %in% retained]))
cat("Lang corrections passed: 3,885 rows, 730 articles, one DOI per article.\n")
