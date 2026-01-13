source("R/match_studyid.R")
source("postprocess.R")

mfl <- load_all_mixtures()

match_matrix(
  list(
    bear_list$JagerLeek,
    bear_list$Head,
    bear_list$Chavalarias,
    bear_list$BarnettWren
  )
)

scrape <- c("JagerLeek", "Head", "Chavalarias", "BarnettWren")
mfl[scrape]

matched_dfs <- list(
  chv_jl = bear_list$Chavalarias %>% 
    filter(studyid %in% unique(bear_list$JagerLeek$studyid)),
  chv_head = bear_list$Chavalarias %>%
    filter(studyid %in% unique(bear_list$Head$studyid)),
  chv_bw = bear_list$Chavalarias %>%
    filter(studyid %in% unique(bear_list$BarnettWren$studyid))
) %>% lapply(thin_df)

matched_fits <- lapply(matched_dfs, fit_mixture_df)

names(matched_fits) <- paste0("to ", c("Jager Leek", "Head", "Barnett Wren"))

# Mixture density
mix_pdf <- function(z, fit)  {
  omega=fit$omega[1]
  B1=2*pmix(-1.96,p=fit$p,m=fit$m,s=fit$sigma) # prob. |z|>1.96
  B2=1-B1
  2*(omega*(z<1.96) + (z>=1.96))*
    dmix(z, p=fit$p, m=fit$m, s=fit$sigma)/(B1 + omega*B2)
}

mix_cdf <- function(z, fit) {
  omega <- fit$omega[1]
  B1 <- 2 * pmix(-1.96, p = fit$p, m = fit$m, s = fit$sigma) 
  B2 <- 1 - B1
  
  F_abs <- function(x)
    2 * pmix(x, p = fit$p, m = fit$m, s = fit$sigma) - 1
  
  out <- numeric(length(z))
  low <- z < 1.96                   # 0 ≤ |z| < 1.96
  hi  <- !low                       # |z| ≥ 1.96
  out[low] <- omega * F_abs(z[low])
  out[hi]  <- omega * B2 + (F_abs(z[hi]) - F_abs(1.96))
  out / (B1 + omega * B2)
}


x_z <- seq(0, 10, length = 1000)
c(mfl[scrape], matched_fits) %>% 
  lapply(function(fit) mix_pdf(x_z, fit)) %>% 
  data.frame() %>% 
  mutate(z = x_z) %>% 
  gather(dataset, value, -z) %>% 
  ggplot() +
  geom_line(aes(x=z,y=value, color=dataset)) +
  xlab("absolute z-statistic") + ylab('') + 
  xlim(0,10) + theme_bw()



# What if we only look at abstracts?

df_abs <- list(
  Chava = bear_list$Chavalarias %>% filter(source == "MEDLINE"),
  Head = bear_list$Head %>% filter(source == "abstract")
)
df_abs[["ChavaHead"]] <- df_abs$Chava %>% filter(studyid %in% unique(df_abs$Head$studyid))
abstract_thin <- lapply(df_abs, thin_df)
abstract_fits <- lapply(abstract_thin, fit_mixture_df)
