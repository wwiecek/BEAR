
collect_results = function(fit,db="CDSR",studies,zstats,signif,power,sgn){
  result=data.frame(db,studies,zstats,signif)
  A=as.vector(as.matrix(fit))
  names(A)=paste0(rep(colnames(fit),each=4),1:4)
  result=cbind(result,t(A))
  result$mean_pow=mean(power)
  result$median_pow=median(power)
  result$pow80=mean(power >= 0.8)
  result$pow90=mean(power >= 0.9)
  result$repl=gap(1.96,p=fit$p,m=fit$m,s=fit$sigma_SNR)$rep
  result$mean_sgn=mean(df$sgn)
  result$median_sgn=median(df$sgn)
  result$sgn80=mean(df$sgn >= 0.8)
  result$sgnl90=mean(df$sgn >= 0.9)
  result$sgn=gap(1.96,p=fit$p,m=fit$m,s=fit$sigma_SNR)$sgn
  return(result)
}

load_all_mixtures <- function() {
  mfl <- list()
  nms <- gsub(".rds", "", list.files("results/mixtures/"))
  for(nm in nms) {
    fnm <- paste0("results/mixtures/", nm, ".rds")
    mfl[[nm]] <- readRDS(fnm)
  }
  mfl
}

calc_study_weights <- function(df){
  df %>% 
  group_by(metaid, studyid) %>% 
  mutate(k = n()) %>% 
  ungroup() %>% 
  mutate(weights = 1/k) 
}
