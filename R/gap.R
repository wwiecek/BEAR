# Replication and correct sign given |z|

# We compute the conditional probability of the correct sign and of successful replication 
# (i.e $p<0.05$ in the same direction as the original study) given the absolute $z$-statistic. 

# Assume SNR ~ dmix(p,m,s) with default values from @van2021statistical 


gap = function(z,
               p=c(0.32,0.31,0.3,0.07),
               m=rep(0,4),
               s=c(0.61,1.42,2.16,5.64)){
  post=posterior(abs(z),p,m,s)
  sgn=1-pmix(0,p=post$p,m=post$pm,s=post$ps)
  rep=1-pmix(1.96,p=post$p,m=post$pm,s=sqrt(post$pv+1))
  data.frame(sgn,rep)
}


# Conditional distribution of SNR given z, when SNR ~ dmix(p,m,s)

posterior <- function(z,p,m,s) {
  s2=s^2
  p=p*dnorm(z,m,sqrt(s2+1))
  p <- p/sum(p)                  # conditional mixing probs
  pm <- z*s2/(s2+1) + m/(s2+1)   # conditional means
  pv <- s2/(s2+1)                # conditional variances
  ps <- sqrt(pv)                 # conditional std devs
  data.frame(p,pm,pv,ps)
}



# gap_plot = function(p,m,s){
gap_plot = function(fit, pow = NA){
  z_grid <- data.frame(z=seq(0,5,0.1))
  
  df0 <- z_grid %>% 
    rowwise() %>% 
    mutate(prob=gap(z)) %>%
    unnest_wider(prob) %>% 
    pivot_longer(cols=c('sgn', 'rep'),
                 names_to='label',
                 values_to='prob')
  
  df <- z_grid %>% 
    rowwise() %>% 
    mutate(prob=gap(z = z, p = fit$p, m = fit$m, s = fit$sigma_SNR)) %>%
    unnest_wider(prob) %>% 
    pivot_longer(cols=c('sgn', 'rep'),
                 names_to='label',
                 values_to='prob') %>% 
    mutate(label=recode_factor(label,
                               "rep" = "successful replication", 
                               "sgn" = "correct sign")) %>% 
    mutate(prior = "Empirical prior")
  
  
  # snr=rmix(10^5,p=p,m=m,s=s)
  # power=pnorm(-1.96,snr,1) + 1 - pnorm(1.96,snr,1)
  # pow=mean(power)
  ggp=ggplot(df,aes(x=z,y=prob,group=label)) + 
    geom_abline(intercept=pow,slope=0,linetype="dashed",alpha=0.5) +
    geom_line() + 
    geom_textline(aes(label = label),vjust=2.5,size=3.5) +
    scale_y_continuous(minor_breaks = seq(0,1,0.05),
                       breaks = seq(0,1,0.1),lim=c(0,1)) +
    scale_x_continuous(minor_breaks = seq(0,5,0.5),
                       breaks = seq(0,5,1),lim=c(0,5)) +
    ylab("") + xlab("|z-statistic|") +
    geom_line(data=df0,aes(x=z,y=prob,group=label),lwd=1,alpha=0.15) +
    theme_bw()
  print(ggp)
}
