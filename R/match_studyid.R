library(data.table)
library(Matrix)

# counting overlaps in studyid's for datasets that use PMIDs (chatGPT function)

match_matrix <- function(dfs, id_col = "studyid", return_percent = TRUE) {
  n  <- length(dfs)
  stopifnot(n >= 2L)
  
  ## 1. stack & deduplicate --------------------------------------------------
  dt <- rbindlist(
    Map(function(d, i) setDT(d)[, .(id = get(id_col), ds = i)],
        dfs, seq_len(n)),
    use.names = FALSE
  )[
    , .SD[!duplicated(id)], by = ds        # one (id,ds) per row
  ][
    , id_int := .GRP, by = id              # contiguous ints, faster than factor()
  ]
  
  ## 2. sparse 0/1 incidence matrix -----------------------------------------
  X <- sparseMatrix(i = dt$id_int,
                    j = dt$ds,
                    x = 1L,
                    dims = c(max(dt$id_int), n),
                    dimnames = list(NULL, paste0("D", seq_len(n))))
  
  ## 3. overlaps -------------------------------------------------------------
  counts <- as.matrix(crossprod(X))
  if (!return_percent) return(counts)
  
  pct <- sweep(counts, 1, diag(counts), "/")
  list(counts = counts, percent = pct)
}


