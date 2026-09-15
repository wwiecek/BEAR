library(tidyverse)
source("R/doi_lookup.R")
metapsy <- readRDS("data_raw/Metapsy/data/Metapsy_Jan2026.rds") %>%
  lapply(function(df) {
    
    measure <- "Hedges' g"
    
    if(is.null(df$.g)) {
      measure <- "SMD"
      # for total-response dataset, I calculate log(OR) and convert to SMD
      # (there is a shorter way to do it, but I didn't notice initially that
      #  there are raw counts in this dataset)
      df <- df %>% 
        mutate(logor =     plogit.ig - plogit.cg,
               se.logor =  sqrt(se.plogit.ig^2 + se.plogit.cg^2)) %>% 
        mutate(.g  = logor/(pi/sqrt(3)),
               .g_se = se.logor/(pi/sqrt(3)))
    }
    
    if(all(c("n_arm1", "n_arm2") %in% colnames(df))) df$ss <- df$n_arm1 + df$n_arm2
    else df$ss <- NA
    
    if (all(c("study", ".g", ".g_se") %in% colnames(df))) {
      ret <- df[, c("study", ".g", ".g_se", "ss"), drop = FALSE]
      ret$reference <- coalesce(!!!lapply(
        c("full_ref", "full_reference", "reference"),
        function(field) if (is.null(df[[field]])) NA_character_
        else na_if(df[[field]], "")))
      ret$doi <- coalesce(extract_doi(if (is.null(df$doi)) NA_character_
                                    else df$doi), extract_doi(ret$reference))
      ret$measure <- measure
      ret$measure_detailed <- if (measure == "Hedges' g") {
        "SMD (Hedges' g)"
      } else {
        "SMD (from log odds ratio)"
      }
      if(!is.null(df$year))
        ret$year <- df$year
      else #this happens in suicide-psyctr
        ret$year <- as.numeric(sub(".*?(\\d{4}).*?$", "\\1", df$study))
      return(ret)
    } else {
      # total-response
      return(data.frame())
    }
  }) %>% 
  bind_rows(.id = "metaid") %>% 
  as_tibble()
doi_mapping <- if (file.exists("doi/Metapsy/final/doi_mapping.rds")) {
  readRDS("doi/Metapsy/final/doi_mapping.rds")
} else tibble(metaid = character(), study = character(),
              reference = character(), doi = character())
if (file.exists("doi/Metapsy/final/manual_doi_map.csv")) {
  manual_mapping <- read_csv("doi/Metapsy/final/manual_doi_map.csv",
                             show_col_types = FALSE)
  stopifnot(!anyDuplicated(manual_mapping[c("metaid", "study", "reference")]))
  doi_mapping <- doi_mapping %>% select(metaid, study, reference,
                                         lookup_doi = doi) %>%
    full_join(manual_mapping, by = c("metaid", "study", "reference")) %>%
    mutate(doi = coalesce(doi, lookup_doi)) %>% select(-lookup_doi)
}
if (nrow(doi_mapping)) {
  metapsy <- join_identifiers(metapsy,
    doi_mapping, c("metaid", "study", "reference"), "doi")
}
metapsy <- select(metapsy, -reference)
saveRDS(metapsy, "data/Metapsy.rds")
