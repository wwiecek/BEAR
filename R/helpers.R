
load_all_mixtures <- function(exclude = NULL) {
  mfl <- list()
  nms <- gsub(".rds", "", list.files("mixtures/"))
  if(!is.null(exclude))
    nms <- nms[!(nms %in% exclude)]
  for(nm in nms) {
    fnm <- paste0("mixtures/", nm, ".rds")
    mfl[[nm]] <- readRDS(fnm)
  }
    
  mfl
}

calc_study_weights <- function(df){
  df %>% 
  group_by(metaid, studyid) %>% 
  mutate(k = n()) %>% 
  ungroup() %>% 
  mutate(weights = 1/k) 
}

# This mini function tries to deal with very, very small p values and 
# also performs a bit of truncation by default
z_from_p <- function(p, truncate = 100) {
  z <- qnorm(1 - p/2)
  # extra precision recommended by chatGPT
  small_p <- !is.na(p) & p < 1e-15
  z[small_p] <- sqrt(-2 * log(p[small_p]/2))
  z[!is.na(p) & p <= 0] <- truncate
  z[!is.na(p) & p >= 1] <- 0
  z
}