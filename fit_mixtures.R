library(tidyverse)
library(tictoc)
library(digest)
source("R/helpers.R")
source("R/settings.R")
source("R/mix.R")
# source("R/mix_v2.R") #faster alternatives!

# bear <- readRDS("data/BEAR.rds")

load("transformed_data/bear_lists.Rdata")

# previous_hash <- readRDS("results/mixtures_hash.rds")
# for(nm in names(bear_hash)){
#   if(bear_hash[[nm]] != previous_hash[[nm]])
#     cat(nm)
# }

# Nelder-Mead optimisation we use here will take minutes per dataset
mtofit <- names(bear_list_thin)

for(nm in mtofit) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  # if(!file.exists(fnm) || !(nm %in% names(previous_hash)) || previous_hash[[nm]] != bear_hash[[nm]]) {
  if(!file.exists(fnm)) {
    cat(nm); cat("\n")
    tic()
    df <- bear_list_thin[[nm]]
    cfit <- fit_mixture(z = df$z, 
                        operator = df$z_operator,
                        weight = df$weights)
    saveRDS(cfit, fnm)
    toc()
  }
}


saveRDS(bear_hash, file="results/mixtures_hash.rds")




# Plot some figures of mixtures -----

# Years of studies (after thinning)

bind_rows(bear_list_thin) %>% 
  ggplot(aes(year)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") + 
  coord_cartesian(xlim = c(1980, 2025)) +
  ylab("N studies (after thinning)") + xlab("year (of study/publication)")

ggsave(width = 5, height = 5, file = "results/years_thinned.pdf")


# Look at all the plots
pl <- list()
# read mixtures
nms <- gsub(".rds", "", list.files("results/mixtures/"))
mfl <- load_all_mixtures()

for(nm in names(bear_list_thin)) {
  pl[[nm]] <- plot_mixture(mfl[[nm]],
                           bear_list_thin[[nm]]$z,
                           bear_list_thin[[nm]]$weight)
}
library(gridExtra)
combined_plot <- arrangeGrob(grobs = Map(function(p, name) {
  p + ggtitle(bear_names[[name]])
}, pl, names(pl)), ncol = 5)

# ggsave("results/mixtures_plot.pdf", combined_plot, width = 16, height = 9)
ggsave("results/mixtures_plot.pdf", combined_plot, width = 11.69 - 2, height = 8.27 - 1,  # A4 landscape in inches
       units = "in", dpi = 300, device = cairo_pdf)
