library(dplyr)
library(readxl)
library(googlesheets4)
library(writexl)
library(knitr)
library(kableExtra)
library(scales)

source("R/settings.R")
bear <- readRDS("data/BEAR.rds")


url <- "https://docs.google.com/spreadsheets/d/1x2A4pgNDfrXRTdzI_LX219twXDXPPPiS7DRw1s6dHyo/edit?gid=0#gid=0"
df_info_raw <- read_sheet(url)
# df_info_raw <- read_excel("data/BEAR datasets summary.xlsx")
df_info <- df_info_raw %>% filter(!is.na(Year)) %>% select(1:4)
bear_summary <- bear %>% 
  group_by(dataset) %>% 
  summarise(n_z = n(), 
            n_meta = n_distinct(metaid), 
            n_study = n_distinct(studyid), 
            mean_k  = n()/n_distinct(studyid), 
            pct_signif = round(100*(sum(abs(z)>1.96)/n()))
  )


df_tex <- left_join(df_info %>% select(1,2,4), 
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
        col.names = c("Dataset", "Year", "Purpose of dataset", 
                      "N studies", "k", "pct signif."),
        align = c("p{2.5cm}", "r", "p{6cm}", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines("paper/tables/table1.tex")
