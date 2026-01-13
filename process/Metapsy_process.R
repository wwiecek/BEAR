library(tidyverse)
readRDS("data_raw/Metapsy/Metapsy_Jan2026.rds") %>% 
  lapply(function(df) {
    if(is.null(df$.g)) {
      # for total-response dataset, I calculate log(OR) and convert to SMD
      # (there is a shorter way to do it, but I didn't notice initially that
      #  there are raw counts in this dataset)
      df <- df %>% 
        mutate(logor =     plogit.ig - plogit.cg,
               se.logor =  se.plogit.ig^2 + se.plogit.cg^2) %>% 
        mutate(.g  = logor/(pi/sqrt(3)),
               .g_se = se.logor/(pi/sqrt(3)))
    }
    
    if(all(c("n_arm1", "n_arm2") %in% colnames(df))) df$ss <- df$n_arm1 + df$n_arm2
    else df$ss <- NA
    
    if (all(c("study", ".g", ".g_se") %in% colnames(df))) {
      ret <- df[, c("study", ".g", ".g_se", "ss"), drop = FALSE]
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
  as_tibble() %>% 
  saveRDS("data/Metapsy.rds")
