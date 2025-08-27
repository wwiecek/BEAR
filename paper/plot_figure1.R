library(tidyverse)
library(patchwork)
source("R/settings.R")
source("R/helpers.R")
source("R/mix.R")
mfl <- load_all_mixtures()
load("data/bear_lists.Rdata")

c_star <- 1.96
grid_x <- seq(0, 10, by = 0.01)

line_df <- function(fit, x = grid_x) {
  omega <- fit$omega[1]
  B1    <- 2 * pmix(-c_star, p = fit$p, m = fit$m, s = fit$sigma)
  B2    <- 1 - B1
  tibble(
    x  = x,
    fz = 2 * (omega * (x < c_star) + (x >= c_star)) *
      dmix(x, p = fit$p, m = fit$m, s = fit$sigma) / (B1 + omega * B2)
  )
}

ds_tbl <- tibble(dataset = names(mfl),
                 group = bear_classification[dataset])

cols_grp <- c(curated = "#E41A1C", meta = "#377EB8", scrape = "#4DAF4A")

lines_all <- imap_dfr(mfl, ~ line_df(.x) %>% mutate(dataset = .y)) |>
  left_join(ds_tbl, by = "dataset") |>
  mutate(dataset = factor(dataset, levels = ds_tbl$dataset),
         group   = factor(group, levels = c("curated","meta","scrape")))

mk_small <- function(ds) {
  d   <- filter(lines_all, dataset == ds)
  grp <- as.character(d$group[1])
  col <- cols_grp[grp]
  dh  <- bear_list_thin[[ds]] |> transmute(x = abs(z), weights = weight)
  
  ggplot() +
    geom_histogram(
      data = dh,
      aes(x = x, y = after_stat(density), weight = weights),
      breaks = seq(0, 10, 0.4),
      fill = col, alpha = 0.25, colour = NA
    ) +
    geom_line(data = d, aes(x = x, y = fz), linewidth = 0.8, colour = col) +
    # geom_vline(xintercept = 1.96, linetype = "dashed", colour = "grey40") +
    coord_cartesian(xlim = c(-0.1, 10.1), ylim = c(0, 0.6)) +
    labs(x = NULL, y = NULL, title = bear_names[ds]) +
    theme_bw() +
    theme(plot.title = element_text(size = 9),
          axis.text  = element_text(size = 7),
          legend.position = "none") + 
    annotate("text", 
             x = 8.5, y = 0.55,   
             label = paste("omega ==", round(mfl[[ds]]$omega[1], 2)),
             parse = TRUE,
             size = 3)
  
}

cur <- ds_tbl %>% filter(group == "curated") %>% pull(dataset)
met <- ds_tbl %>% filter(group == "meta")    %>% pull(dataset)
scr <- ds_tbl %>% filter(group == "scrape")  %>% pull(dataset)

row1  <- c(lapply(cur, mk_small), list(plot_spacer()))
row23 <- { p <- lapply(met, mk_small)
c(p, replicate(max(0, 8 - length(p)), plot_spacer(), simplify = FALSE)) }
row4  <- c(lapply(scr, mk_small))
wrap_plots(c(row1, row23, row4), ncol = 4)

ggsave("results/mixtures_plot_pmixonly.pdf", height = 16, width = 14, units = "cm")
