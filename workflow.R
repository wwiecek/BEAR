# analytical workflow for each dataset
library(tidyverse)

source("R/exaggeration.R")
source("R/gap.R")
source("R/helpers.R")
source("R/mix.R")
source("R/posterior.R")


mfl <- list()
nms <- gsub(".rds", "", list.files("results/mixtures/"))
for(nm in nms) {
  fnm <- paste0("results/mixtures/", nm, ".rds")
  mfl[[nm]] <- readRDS(fnm)
}


N <- 1e04
# power, sign, replication: all in a single data frame
df_psr <- lapply(mfl, function(fit) 
  data.frame(snr = rmix(N,p=fit$p,m=fit$m,s=fit$sigma_SNR)) %>% 
  mutate(z = snr + rnorm(N)) %>% 
  rowwise() %>% 
  mutate(power = pnorm(-1.96, snr, 1) + 1 - pnorm(1.96, snr, 1),
         prob  = gap(z = z, p = fit$p, m = fit$m, s = fit$sigma_SNR)) %>%
  unnest_wider(prob)
)  

# Table with study summaries
tab2 <- bind_rows(df_psr, .id = "dataset") %>% group_by(dataset) %>% 
  summarise(
    mean(power),
    sum(power > .8)/n(),
    # sum(sgn > .8)/n(),
    mean(sgn),
    # sum(rep > .8)/n(),
    mean(rep)) %>% 
  mutate(dataset = bear_names[dataset]) 

tab2 %>% 
  mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Dataset", "Assurance", "Pr(power > 0.8)", "Correct sign", 
                      "Replication"),
        align = c("p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines("results/table2.tex")


bind_rows(df_psr, .id = "dataset") %>% 
  ggplot(aes(power, color = dataset)) + geom_density()
bind_rows(df_psr, .id = "dataset") %>% 
  ggplot(aes(sgn, color = dataset)) + geom_density()

gap_plot(mfl[[5]])

