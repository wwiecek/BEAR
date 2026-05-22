
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

# This mini function tries to deal with very, very small p values.
z_from_p <- function(p) {
  z <- qnorm(1 - p/2)
  # extra precision recommended by chatGPT
  small_p <- !is.na(p) & p < 1e-15
  z[small_p] <- sqrt(-2 * log(p[small_p]/2))
  z[!is.na(p) & p <= 0] <- Inf
  z[!is.na(p) & p >= 1] <- 0
  z
}

# If there are too many rows, first keep one row from as many studies as
# possible, then sample extra rows up to N.
thin_df <- function(df, N = 50000, seed = NULL) {
  if(!is.null(seed)){
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if(had_seed)
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
      if(had_seed)
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  sample_indices <- function(x, size) x[sample.int(length(x), size)]

  if(nrow(df) > N){
    study_indices <- aggregate(
      seq_len(nrow(df)),
      by = list(studyid = df$studyid),
      FUN = function(x) sample_indices(x, 1)
    )$x

    if(length(study_indices) >= N){
      return(df[sample_indices(study_indices, N), ])
    }

    remaining_indices <- setdiff(seq_len(nrow(df)), study_indices)
    extra_n <- min(N - length(study_indices), length(remaining_indices))
    extra_indices <- sample_indices(remaining_indices, extra_n)
    df <- df[c(study_indices, extra_indices), ]
  }

  df
}

