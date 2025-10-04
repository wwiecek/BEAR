rmarkdown::render("README.Rmd", output_format = "github_document")

# Count PMID overlaps
dfs <- list("A" = bear_list$JagerLeek, 
            "B" = bear_list$Chavalarias, 
            "C" = bear_list$BarnettWren)
match_matrix(lst)



# Publication years
bear %>% 
  filter(!is.na(year)) %>% 
  summarise(
    sum(year < 2000)/n(),
    sum(year < 2010)/n(),
    sum(year < 2020)/n())
