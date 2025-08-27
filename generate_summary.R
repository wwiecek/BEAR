rmarkdown::render("README.Rmd", output_format = "github_document")

# Table 1
library(readxl)
df_info_raw <- read_excel("data/BEAR datasets summary.xlsx")
df_info <- df_info_raw %>% filter(!is.na(Year)) %>% select(1:4)

bear_summary <- bear %>% 
  group_by(dataset) %>% 
  summarise(n_z = n(), 
          n_meta = n_distinct(metaid), 
          n_study = n_distinct(studyid), 
          mean_k  = n()/n_distinct(studyid), 
          pct_signif = sum(abs(z)>1.96)/n()
  )


library(knitr)
library(kableExtra)
library(scales)
df_tex <- left_join(df_info, 
                    bear_summary %>% 
                      mutate(Dataset = bear_names[dataset]) %>% 
                      select(Dataset, n_study, mean_k, pct_signif)
)

df_tex %>%
  mutate(
    n_study = comma(n_study),                        # e.g., 1,000,000
    mean_k = round(mean_k, 1),                       # one digit
    pct_signif = round(pct_signif, 2)   # as percent, no decimals
  ) %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Dataset", "Year", "Variable", "Purpose of dataset", 
                      "N studies", "Mean k", "pct signif."),
        align = c("p{2.5cm}", "r", "p{2.5cm}", "p{6cm}", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines("results/table1.tex")

# Count PMID overlaps
dfs <- list("A" = bear_list$JagerLeek, 
            "B" = bear_list$Chavalarias, 
            "C" = bear_list$BarnettWren)
match_matrix(lst)

