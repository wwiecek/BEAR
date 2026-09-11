# Process Kevin Lang's credibility revolution dataset for BEAR.
# The canonical output keeps one row per extracted hypothesis test.

library(tidyverse)
library(haven)
source("R/doi_lookup.R")

lang_path <- "data_raw/Lang/23259data/Data/cr_append.dta"
doi_keys <- c("paper_id", "source_title", "citation", "journal", "year",
              "lang_source")

# Helpers -----

check_value <- function(label, value, expected) {
  if (!identical(as.integer(value), as.integer(expected))) {
    stop(label, " is ", value, ", expected ", expected, call. = FALSE)
  }
}

check_required <- function(data, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing required fields: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }
}

decode_lang_text <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    code_points <- utf8ToInt(s)
    if (any(code_points > 255)) return(s)
    iconv(rawToChar(as.raw(code_points)), from = "MACINTOSH", to = "UTF-8")
  }, character(1), USE.NAMES = FALSE)
}

clean_character <- function(x) {
  na_if(str_squish(decode_lang_text(as.character(x))), "")
}

# Prepare data -----

lang_raw <- read_dta(lang_path, encoding = "latin1")

check_required(lang_raw, c("t", "year", "method", "unique_paperid"))

lang <- lang_raw %>%
  mutate(
    source_row = row_number(),
    estimate_id = str_c("Lang_", str_pad(source_row, 4, pad = "0")),
    paper_id = str_c("Lang_paper_", unique_paperid),
    studyid = paper_id,
    z = t,
    abs_z = abs(z),
    p = 2 * pnorm(-abs_z),
    significant = abs_z >= 1.96,
    lang_main_sample = abs_z >= 1.96 & abs_z < 10,
    lang_source_brodeur = Brodeur == 1,
    lang_source = if_else(lang_source_brodeur,
                          "Brodeur, Cook, and Heyes 2015/2018 source universe",
                          "Lang 2021 top-five additions")
  ) %>%
  transmute(
    dataset = "Lang",
    studyid,
    estimate_id,
    paper_id,
    citation = clean_character(citation),
    journal = clean_character(journal),
    year = as.integer(year),
    method = clean_character(method),
    z,
    abs_z,
    p,
    b = coef_dr,
    se = se_dr,
    significant,
    lang_main_sample,
    lang_source_brodeur,
    lang_source,
    source_file = lang_path,
    source_row,
    source_unique_paperid = unique_paperid,
    source_brodeur = Brodeur,
    source_mht = MHT,
    source_note = clean_character(note),
    source_author1 = clean_character(Author1),
    source_title = clean_character(title),
    source_table = table,
    source_report = clean_character(report),
    source_coef = coef,
    source_coef_derounded = coef_dr,
    source_coefid = coefid,
    source_se = se,
    source_se_derounded = se_dr,
    source_se2 = se2,
    source_se3 = se3,
    source_p_value = p_value,
    source_p_value_derounded = p_value_dr,
    source_p_value2 = p_value2,
    source_t_stat = t_stat,
    source_t_derounded = t,
    source_matlab_sample = matlab_sample
  )

# Bibliographic hotfixes -----
# Evidence and the distinction between source IDs and articles: Lang_doi_notes.md.
# Keep raw files, original labels and every test; consolidate article IDs only.
lang <- lang %>% mutate(
  source_title_original = source_title,
  studyid = case_when(
    source_unique_paperid %in% 2:5 ~ "Lang_paper_2",
    source_unique_paperid %in% 29:31 ~ "Lang_paper_29",
    source_unique_paperid %in% 42:43 ~ "Lang_paper_42",
    TRUE ~ paper_id),
  source_title = case_when(
    source_row %in% c(2052L, 2054L) ~
      source_title[match(2043L, source_row)],
    paper_id == "Lang_paper_223" ~
      "Financial development and the choice of trade partners",
    paper_id == "Lang_paper_324" ~ paste(
      "Women's schooling and fertility under low female labor force",
      "participation: Evidence from mobility restrictions in Israel"),
    TRUE ~ source_title))

# These adjudications override both old caches and subsequent automatic searches.
manual_dois <- c(
  Lang_paper_2 = "10.1257/aer.20191586",
  Lang_paper_29 = "10.1093/qje/qjab016",
  Lang_paper_42 = "10.1257/aer.20201238",
  Lang_paper_100 = "10.1093/qje/qjab004",
  Lang_paper_223 = "10.1016/j.jdeveco.2015.04.002",
  Lang_paper_324 = "10.1016/j.jpubeco.2015.02.009",
  Lang_paper_397 = "10.1257/app.20150245",
  Lang_paper_472 = "10.1111/ecoj.12505",
  Lang_paper_474 = "10.1111/ecoj.12448",
  Lang_paper_530 = "10.1016/j.jdeveco.2018.07.008",
  Lang_paper_597 = "10.1016/j.jinteco.2017.08.002",
  Lang_paper_689 = "10.1093/qje/qjx040",
  # Reviewed low-overlap cases: retain the correct, often shortened titles.
  Lang_paper_208 = "10.1093/epolic/eiv015",
  Lang_paper_279 = "10.3368/jhr.50.4.959",
  Lang_paper_280 = "10.3368/jhr.50.4.1051",
  Lang_paper_570 = "10.1016/j.jfineco.2018.01.008",
  Lang_paper_581 = "10.3368/jhr.53.3.0115.6895r1",
  Lang_paper_584 = "10.3368/jhr.53.2.0816-8112r1",
  Lang_paper_585 = "10.3368/jhr.53.3.0215-6948r4",
  Lang_paper_586 = "10.3368/jhr.53.4.1115.7494r1",
  Lang_paper_589 = "10.3368/jhr.53.2.0115-6868r1",
  Lang_paper_590 = "10.3368/jhr.53.3.0215-6963r1",
  Lang_paper_591 = "10.3368/jhr.53.1.0215-6958r1",
  Lang_paper_627 = "10.1016/j.jpubeco.2018.08.015",
  Lang_paper_641 = "10.1016/j.jpubeco.2018.05.002",
  Lang_paper_656 = "10.1016/j.jpubeco.2018.07.002")

# Validate -----

check_value("Full Lang source rows", nrow(lang), 3885)
check_value("Full Lang source papers", n_distinct(lang$paper_id), 736)
check_value("Distinct Lang articles", n_distinct(lang$studyid), 730)
check_value("Droller rows", sum(lang$studyid == "Lang_paper_472"), 6)
check_value("Jessoe rows", sum(lang$studyid == "Lang_paper_474"), 14)
stopifnot(all(lang$paper_id[lang$source_row %in% c(2052L, 2054L)] ==
                "Lang_paper_474"))
check_value("Lang main sample rows", sum(lang$lang_main_sample), 2082)
check_value("Lang main sample papers",
            n_distinct(lang$paper_id[lang$lang_main_sample]), 663)
check_value("Lang main sample articles",
            n_distinct(lang$studyid[lang$lang_main_sample]), 660)

p_cut_counts <- map_int(c(.04, .03, .02, .01),
                        ~sum(lang$p < .x & lang$lang_main_sample))
expected_p_cut_counts <- c(1963L, 1815L, 1631L, 1384L)
if (!identical(p_cut_counts, expected_p_cut_counts)) {
  stop("Main-sample p-threshold counts are ",
       paste(p_cut_counts, collapse = " / "), ", expected ",
       paste(expected_p_cut_counts, collapse = " / "), call. = FALSE)
}

if (anyDuplicated(lang$estimate_id) > 0) {
  stop("Canonical estimate_id is not unique", call. = FALSE)
}

# Save -----

# Preserve current assignments, including independent manual corrections.
if (file.exists("data/Lang.rds")) {
  previous_lang <- readRDS("data/Lang.rds")
  if ("doi" %in% names(previous_lang)) {
    lang <- join_identifiers(lang, previous_lang, "estimate_id", "doi")
  }
}
if (file.exists("data_raw/Lang/derived/lang_doi_lookup.csv")) {
  lookup <- read_csv("data_raw/Lang/derived/lang_doi_lookup.csv",
                     show_col_types = FALSE)
  # Existing assignments may have been corrected since the mapping was written.
  if ("doi" %in% names(lang)) {
    lookup <- lookup %>% anti_join(filter(lang, !is.na(doi)), by = doi_keys)
  }
  lang <- join_identifiers(lang, lookup, doi_keys, "doi")
} else if (!"doi" %in% names(lang) &&
    file.exists("data_raw/Lang/derived/lang_doi_candidates.csv")) {
  lookup <- read_csv("data_raw/Lang/derived/lang_doi_candidates.csv",
                     show_col_types = FALSE)
  lang <- join_identifiers(lang, lookup, doi_keys, "doi")
}
# Approved journal-version assignments supersede the historical lookup.
if (file.exists("data_raw/Lang/derived/lang_doi_journal_lookup.csv")) {
  lookup <- read_csv("data_raw/Lang/derived/lang_doi_journal_lookup.csv",
                     show_col_types = FALSE) %>%
    filter(status == "matched") %>% select(paper_id, doi)
  stopifnot(!anyDuplicated(lookup$paper_id))
  lang <- lang %>% left_join(rename(lookup, doi_journal = doi), by = "paper_id")
  if (!"doi" %in% names(lang)) lang$doi <- NA_character_
  lang <- lang %>% mutate(doi = coalesce(doi_journal, doi)) %>% select(-doi_journal)
}
if (!"doi" %in% names(lang)) lang$doi <- NA_character_
lang <- lang %>% mutate(doi = coalesce(unname(manual_dois[studyid]), doi))
stopifnot(n_distinct(lang$source_title[lang$studyid == "Lang_paper_474"]) == 1L)
saveRDS(lang, "data/Lang.rds")

# Validation summary -----

cat("Lang validation summary\n")
cat("Full source:", nrow(lang), "tests from", n_distinct(lang$paper_id),
    "source IDs;", n_distinct(lang$studyid), "articles\n")
cat("Main sample:", sum(lang$lang_main_sample), "tests from",
    n_distinct(lang$paper_id[lang$lang_main_sample]), "source IDs;",
    n_distinct(lang$studyid[lang$lang_main_sample]), "articles\n")

cat("Year counts\n")
print(lang %>% count(year, name = "tests") %>% arrange(year))

cat("\nMethod counts\n")
print(lang %>% count(method, name = "tests", sort = TRUE))

cat("\nSource counts\n")
print(lang %>%
        count(lang_source_brodeur, lang_source, name = "tests") %>%
        mutate(papers = map_int(lang_source_brodeur,
                                ~n_distinct(lang$paper_id[
                                  lang$lang_source_brodeur == .x]))))

cat("\nMissing title/author/journal metadata\n")
print(lang %>%
        group_by(lang_source_brodeur, lang_source) %>%
        summarise(
          tests = n(),
          papers = n_distinct(paper_id),
          title_missing = sum(is.na(source_title)),
          author_missing = sum(is.na(source_author1)),
          journal_missing = sum(is.na(journal)),
          .groups = "drop"
        ))
cat("\nNote: the 392 non-Brodeur 2021 rows lack title, author, and journal ",
    "metadata in the supplied source file.\n", sep = "")
