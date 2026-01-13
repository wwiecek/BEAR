dfs <- lapply(list.files("data_raw/Sladekova/", full.names = TRUE), read.csv)
names(dfs) <- list.files("data_raw/Sladekova/")

# sort(table(unlist(lapply(dfs, function(f) names(f)))), decreasing = TRUE)

onedf <- lapply(dfs, function(df){
  
  if(all(c("n_treatment", "n_control") %in% colnames(df))) df$n <- df$n_treatment + df$n_control
  
  ret <- df %>% 
    filter(!is.na(yi) & !is.na(vi)) %>% 
    # Fisher's z won't work otherwise; this affects less than 0.5% of observations
    filter((1 + yi)/(1 - yi) > 0) %>% 
    mutate(
      b = 0.5*log((1 + yi)/(1 - yi)),
      se = sqrt(vi)) 
  
  # Trying to count sample sizes where possible
  
  if(nrow(ret) == 0)
    return(data.frame())
  # Study column is broken, as a temporary fix I need to assign new numerical study IDs
  # if(!is.null(ret$author)) ret$studyid <- as.character(ret$author) else ret$studyid <- as.character(1:nrow(ret))
  if(is.null(ret$year)) ret$year <- as.numeric(NA) else ret$year <- as.numeric(ret$year)
  if(is.null(ret$n)) ret$ss <- as.numeric(NA) else ret$ss <- as.numeric(ret$n)
  
  ret %>% 
    select(b, se, year, ss)
}) %>% 
  bind_rows(.id = "metaid")

saveRDS(onedf, "data/Sladekova.rds")
rm(onedf)
rm(dfs)
