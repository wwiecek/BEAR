# chatGPT script WW created to grab PMIDs that can be joined to DOI's

library(europepmc)   # epmc_search()
library(pbapply)     # pbsapply() progress bar
library(data.table)  # fast I/O and binding

## ---- YOUR INPUT VECTOR -----------------------------------------------
# Example: pull unique DOIs from your 'head' data frame
head <- read_csv("data_raw/Head/p_values_cleaned_ww.csv")
hdoi <- unique(head$first.doi)

## ---- CONFIG -----------------------------------------------------------
chunksize   <- 1000              # how many DOIs per save‑point
max_tries   <- 3                 # retries for transient errors
wait_secs   <- 2                 # pause between retries
sleep_after <- 1                 # politeness pause after each batch
out_rds     <- "data_raw/Head/doi2pmid_progress.rds"
out_csv     <- sub("\\.rds$", ".csv", out_rds)
log_file    <- "data_raw/Head/doi2pmid_log.txt"


## ---- RESUME OR START FRESH -------------------------------------------
if (file.exists(out_rds)) {
  progress  <- readRDS(out_rds)
  done      <- progress$doi
  remaining <- setdiff(hdoi, done)
  message("Resuming; ", length(done), " already done, ",
          length(remaining), " remaining.")
} else {
  progress  <- data.table(doi = character(), pmid = character())
  remaining <- hdoi
  message("Starting fresh run with ", length(remaining), " DOIs.")
}

## ---- HELPER WITH RETRIES ---------------------------------------------
doi2pmid_epmc <- function(doi) {
  out <- epmc_search(query = paste0("DOI:", doi),
                     limit = 1, synonym = FALSE, verbose = FALSE)
  if (nrow(out) && out$source[1] == "MED") out$id[1] else NA_character_
}

doi2pmid_safe <- function(doi) {
  for (i in seq_len(max_tries)) {
    res <- tryCatch(doi2pmid_epmc(doi),
                    error = function(e) NA_character_)
    if (!is.na(res) || i == max_tries) return(res)
    Sys.sleep(wait_secs)
  }
}

## ---- MAIN LOOP --------------------------------------------------------
while (length(remaining) > 0) {
  batch <- head(remaining, chunksize)
  
  pmids <- pbsapply(batch, doi2pmid_safe)
  
  progress <- rbindlist(
    list(progress,
         data.table(doi = batch, pmid = pmids)),
    use.names = TRUE)
  
  ## save every chunk – cheap insurance
  saveRDS(progress, out_rds)
  fwrite(progress, out_csv)
  
  ## simple timestamped log
  cat(sprintf("[%s] processed %d / %d DOIs\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              nrow(progress), length(hdoi)),
      file = log_file, append = TRUE)
  
  ## update remaining set
  remaining <- setdiff(remaining, batch)
  
  Sys.sleep(sleep_after)   # be polite to the API
}

message("🎉 All done – results in ", out_csv)
