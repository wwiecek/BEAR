# Write optional derived outputs for the site and rendered project pages.
# Run from the BEAR project root after BEAR.rds, mixture fits, and PSR summaries
# are available.

source("workflow/write_site_metrics.R")
write_site_metrics()

source("workflow/write_selection_plot.R")
source("workflow/write_mixture_plots.R")

# Refresh README
rmarkdown::render(
  "README.Rmd",
  output_format = rmarkdown::github_document(html_preview = FALSE)
)
