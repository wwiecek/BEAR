# analytical workflow for each dataset
library(tidyverse)
library(ggplot2)

source("R/settings.R")
source("R/exaggeration.R")
source("R/gap.R")
source("R/helpers.R")
source("R/mix.R")


mfl <- load_all_mixtures()

# Calculate the derived quantities -----

#will take a while to calculate for 100,000 rows per dataset without vectorising gap()

df_psr <- lapply(mfl, psr_foo) %>% bind_rows(.id = "dataset")
df_psr_169 <- lapply(mfl, psr_foo, 1.69) %>% bind_rows(.id = "dataset")

# saveRDS(df_psr, file = "results/power_sign_replication.rds")


# Table with study summaries ------
summarise_psr <- function(df) 
  df %>% 
  group_by(dataset) %>% 
  summarise(
    p = mean(power),
    p80 = sum(power > .8)/n(),
    s = mean(sgn),
    r = mean(rep)) %>% 
  mutate(dataset = bear_names[dataset])

tab2 <- summarise_psr(df_psr)

left_join(summarise_psr(df_psr),
          summarise_psr(df_psr_169), 
          by = "dataset") %>% 
  mutate(diff_p = p.x - p.y,
         diff_r = r.x - r.y) %>% 
  mutate_if(is.numeric, function(x) scales::percent(as.numeric(x), accuracy = 1))

tab2 %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Dataset", "Assurance", "Pr(power > 0.8)", "Correct sign", 
                      "Replication"),
        align = c("p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines("results/table2.tex")





# Power plots -----

library(ggridges)
library(forcats)

ggplot(df_psr,
       aes(x = rep,
           y = fct_reorder(dataset, rep, .fun = median, .desc = FALSE),
           fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient(
    scale = 2.1, rel_min_height = 0.01, size = 0.25, jittered_points = FALSE
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  guides(fill = "none") +
  labs(x = "Power", y = NULL) +
  theme_ridges(center_axis_labels = TRUE)

