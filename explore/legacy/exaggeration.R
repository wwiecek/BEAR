# We compute the conditional quartiles of the exaggeration ratio $|z/SNR|$ given the absolute $z$-statistic. Assume SNR ~ dmix(p,m,s)

exaggeration = function(z,p,m,s){
  post=posterior(abs(z),p,m,s)
  q25=abs(z)/qmixabs(q=0.25,p=post$p,m=post$pm,post$ps)
  q50=abs(z)/qmixabs(q=0.50,p=post$p,m=post$pm,post$ps)
  q75=abs(z)/qmixabs(q=0.75,p=post$p,m=post$pm,post$ps)
  data.frame(q25,q50,q75)
}

exaggeration_plot = function(p,m,s){
  df=data.frame(z=seq(0,5,0.01))
  df=df %>% rowwise() %>% 
    mutate(R=exaggeration(z,p=p,m=m,s=s)) %>%
    unnest_wider(R)
  
  df=df %>% pivot_longer(cols=c("q25", "q50", "q75"),
                         names_to='label',
                         values_to='R')
  
  ggp=ggplot(df,aes(x=abs(z),y=R, group=label)) + geom_line() + 
    geom_hline(yintercept = 1) +
    scale_y_continuous(minor_breaks = seq(0,5,0.25),
                       breaks = seq(0,5,0.5),lim=c(0,5)) +
    scale_x_continuous(minor_breaks = seq(0,5,0.25), 
                       breaks = seq(0,5,1),lim=c(0,5)) +
    ylab('Exaggeration') + xlab("|z-statistic|") +
    theme_bw()
  print(ggp)
}
