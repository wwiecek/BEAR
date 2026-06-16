# Copy canonical mixture plot artifacts into the Quarto site asset tree.

dataset_classification_file <- "../doc/dataset_classification.csv"
source("../R/settings.R", local = TRUE)
source("../R/site_dataset_config.R", local = TRUE)

source_dir <- "../results/mixture_plots"
output_dir <- "assets/mixture_plots"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

plot_index <- site_dataset_plot_index("../doc/datasets")
expected_files <- unique(plot_index$plot_file)
missing_files <- expected_files[!file.exists(file.path(source_dir, expected_files))]

if (length(missing_files) > 0) {
  stop("Missing canonical mixture plots in ", source_dir, ": ",
       paste(missing_files, collapse = ", "))
}

copied <- file.copy(
  from = file.path(source_dir, expected_files),
  to = file.path(output_dir, expected_files),
  overwrite = TRUE
)

if (!all(copied)) {
  stop("Failed to copy mixture plots: ",
       paste(expected_files[!copied], collapse = ", "))
}

message("Synced ", length(expected_files), " mixture plots.")
