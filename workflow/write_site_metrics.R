# Write landing-page metrics for the Quarto site.
# Run from the BEAR project root after BEAR.rds has been built.

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)

fmt_mln <- function(x) {
  paste0(format(round(x / 1e6, 1), nsmall = 1), " mln")
}

calculate_site_metrics <- function(bear = readRDS("BEAR.rds"),
                                   data_dir = "data") {
  data_files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)
  data_points <- if (length(data_files) > 0) {
    sum(vapply(data_files, function(path) nrow(readRDS(path)), integer(1)))
  } else {
    nrow(bear)
  }

  list(
    data_points = data_points,
    meta_analyses = length(unique(na.omit(bear$metaid))),
    datasets = length(unique(bear$dataset))
  )
}

write_site_metrics <- function(metrics = calculate_site_metrics(),
                               include_file = "site/_site_metrics.md",
                               artifact_file = "results/site_metrics.rds") {
  dir.create(dirname(include_file), showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(artifact_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(metrics, artifact_file)

  metrics_lines <- c(
    "::: {.bear-metrics}",
    paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
           fmt_mln(metrics$data_points), "</div><div class=\"bear-metric-label\">",
           "data points</div></div>"),
    paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
           fmt_int(metrics$meta_analyses),
           "</div><div class=\"bear-metric-label\">meta-analyses</div></div>"),
    paste0("<div class=\"bear-metric\"><div class=\"bear-metric-value\">",
           fmt_int(metrics$datasets),
           "</div><div class=\"bear-metric-label\">datasets</div></div>"),
    ":::"
  )

  writeLines(metrics_lines, include_file)
  invisible(metrics)
}

if (sys.nframe() == 0) write_site_metrics()
