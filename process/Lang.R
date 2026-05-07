# Process Kevin Lang's credibility revolution dataset for BEAR.
# The canonical output keeps one row per extracted hypothesis test.

library(tidyverse)
library(haven)

lang_path <- "data_raw/Lang/23259data/Data/cr_append.dta"

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

clean_character <- function(x) {
  na_if(str_squish(as.character(x)), "")
}

# Prepare data -----

lang_raw <- read_dta(lang_path)

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

# Validate -----

check_value("Full Lang source rows", nrow(lang), 3885)
check_value("Full Lang source papers", n_distinct(lang$paper_id), 736)
check_value("Lang main sample rows", sum(lang$lang_main_sample), 2082)
check_value("Lang main sample papers",
            n_distinct(lang$paper_id[lang$lang_main_sample]), 663)

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

saveRDS(lang, "data/Lang.rds")

# Validation summary -----

cat("Lang validation summary\n")
cat("Full source:", nrow(lang), "tests from", n_distinct(lang$paper_id),
    "papers\n")
cat("Main sample:", sum(lang$lang_main_sample), "tests from",
    n_distinct(lang$paper_id[lang$lang_main_sample]), "papers\n")

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
