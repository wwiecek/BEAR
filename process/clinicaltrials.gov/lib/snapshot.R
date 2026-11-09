# Snapshot identity, cache invalidation and snapshot-specific counts for the
# ClinicalTrials.gov workflow. Sourced by paths.R.

# Identify a snapshot by file name, size and modification time. Hashing a
# multi-gigabyte zip at every step would be slow.
ctgov_snapshot_id <- function(zip = ctgov_snapshot_zip) {
  info <- file.info(zip)
  paste(
    basename(zip),
    format(info$size, scientific = FALSE),
    format(info$mtime, "%Y-%m-%dT%H:%M:%S"),
    sep = "|"
  )
}

ctgov_snapshot_stamp_path <- file.path(ctgov_derived_dir, "snapshot_stamp.txt")

# Derived files that later steps reuse instead of rebuilding. They must come
# from the current snapshot, so they are removed when the snapshot changes.
ctgov_cached_paths <- function() {
  c(
    list.files(ctgov_intermediate_dir, full.names = TRUE),
    file.path(ctgov_derived_dir, c(
      "author_reported_raw.rds",
      "clinicaltrials.gov_aug2025.rds"
    ))
  )
}

# Remove cached derived files when the snapshot differs from the one recorded
# in the stamp (or no stamp exists), then record the current snapshot. Changes
# to extraction code are not detected; clear derived/intermediate/ by hand then.
ctgov_reset_caches_if_snapshot_changed <- function() {
  current <- ctgov_snapshot_id()
  previous <- if (file.exists(ctgov_snapshot_stamp_path)) {
    readLines(ctgov_snapshot_stamp_path, warn = FALSE)[1]
  } else {
    NA_character_
  }
  if (identical(previous, current)) return(invisible(FALSE))

  cached <- ctgov_cached_paths()
  cached <- cached[file.exists(cached)]
  message(
    "Snapshot ", current, " differs from the cached one (",
    ifelse(is.na(previous), "none recorded", previous), "); removing ",
    length(cached), " cached derived files so they are rebuilt."
  )
  unlink(cached, recursive = TRUE)
  writeLines(current, ctgov_snapshot_stamp_path)
  invisible(TRUE)
}

ctgov_counts_path <- file.path(ctgov_validation_dir, "snapshot_counts.csv")

# Record snapshot-specific counts and report changes against the most recent
# run on a different snapshot. Counts are expected to change between snapshots,
# so this never stops the pipeline. Re-running on the same snapshot replaces
# that snapshot's rows for the step.
ctgov_record_counts <- function(step, counts) {
  snapshot <- ctgov_snapshot_id()
  new <- data.frame(
    snapshot = snapshot,
    recorded = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    step = step,
    metric = names(counts),
    value = as.numeric(counts),
    stringsAsFactors = FALSE
  )
  old <- if (file.exists(ctgov_counts_path)) {
    utils::read.csv(ctgov_counts_path, stringsAsFactors = FALSE)
  } else {
    new[0, ]
  }
  replaced <- old$snapshot == snapshot & old$step == step
  earlier <- old[!replaced & old$step == step, ]

  for (i in seq_len(nrow(new))) {
    prev <- earlier[earlier$metric == new$metric[i], ]
    if (nrow(prev) == 0) next
    prev <- prev[order(prev$recorded, decreasing = TRUE)[1], ]
    if (!isTRUE(all.equal(prev$value, new$value[i]))) {
      message(
        "  ", step, ": ", new$metric[i], " = ",
        format(new$value[i], big.mark = ","), " (previous snapshot ",
        prev$snapshot, ": ", format(prev$value, big.mark = ","), ")"
      )
    }
  }
  utils::write.csv(rbind(old[!replaced, ], new), ctgov_counts_path,
                   row.names = FALSE)
  invisible(new)
}

# A count recorded earlier in this snapshot's run, or NA if none.
ctgov_recorded_count <- function(step, metric) {
  if (!file.exists(ctgov_counts_path)) return(NA_real_)
  x <- utils::read.csv(ctgov_counts_path, stringsAsFactors = FALSE)
  x <- x[x$snapshot == ctgov_snapshot_id() & x$step == step &
           x$metric == metric, ]
  if (nrow(x) == 0) NA_real_ else x$value[nrow(x)]
}
