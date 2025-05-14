## Fit mixture distribution
# We fit a 4-part zero-mean normal mixture distribution to the $z$-statistics. Or rather, we fit a mixture of half-normals to the absolute $z$-statistics.

# * We assume that $z$-statistics less than 1.96 may have smaller probability of being observed due to publication bias and/or the file drawer effect. We use a parameter $\omega$ to represent the relative risk:
# $$\omega = \frac{P(\text{observe} \mid |z| < 1.96)}{P(\text{observe} \mid |z| \geq 1.96)}.$$

# * We weight each $z$-statistic by the inverse of the number of $z$-statistics in the same study.

# * We also handle truncation. Truncated z-stats below 1.96 are assume to be truncated above, and truncated z-stats above 1.96 are assume to be truncated below. 

# log likelihood function
loglik = function(theta,z,trunc,k,weights){
  p=c(theta[1:(k-1)],1-sum(theta[1:(k-1)]))
  s=theta[k:(2*k-1)]
  omega=theta[2*k]
  m=rep(0,k)
  B1=1-pmixabs(1.96,p,m,s)    # prob. |z|>1.96
  B2=1-B1                     # prob. |z|<1.96
  lik1=(abs(z)< 1.96)*(trunc=="truncated")*
    omega*pmixabs(abs(z),p,m=m,s=s)
  lik2=(abs(z)< 1.96)*(trunc=="not truncated")*
    omega*dmixabs(abs(z),p,m=m,s=s)
  lik3=(abs(z) >= 1.96)*(trunc=="not truncated")*
    dmixabs(abs(z),p,m=m,s=s)
  lik4=(abs(z) >= 1.96)*(trunc=="truncated")*
    (1-pmixabs(abs(z),p,m=m,s=s))
  lik=(lik1+lik2+lik3+lik4)/(B1 + omega*B2)
  return(-sum(weights*log(lik)))   # *minus* the weighted log lik
}

mix = function(z,trunc,k=4,weights=1){
  # The feasible region is defined by ui %*% par - ci >= 0
  ui=c(rep(-1,(k-1)),rep(0,k),0)         # (k-1) mixture props sum to < 1
  ui=rbind(ui,cbind(diag(2*k)))          # k stdevs > 1 and and omega > 0
  ci=c(-1,rep(0,k-1),rep(1,k),0)
  # set starting value and optimize 
  theta0=c(rep(1/k,(k-1)),c(1.2,2:k),1)
  opt=constrOptim(theta=theta0,f=loglik,ui=ui,ci=ci,
                  method = "Nelder-Mead",
                  z=abs(z),weights=weights,trunc=trunc,k=k,
                  control=list(maxit=10^4))
  # collect the results
  p=c(opt$par[1:(k-1)],1-sum(opt$par[1:(k-1)]))  # mixture proportions
  sigma=opt$par[k:(2*k-1)]                       # mixture sds
  omega=opt$par[2*k]
  m=rep(0,k)                                     # mixture means
  df=data.frame(p=p,m=m,sigma=sigma,omega=omega)
  return(df)
}

fit_mixture <- function(z,truncated,k=4,weights){
  z.trunc=abs(z)
  ind=which(z.trunc>20)
  z.trunc[ind]=20
  # truncated[ind]="truncated"  # commented out to save time
  ind=which(z.trunc==0)
  z.trunc[ind]=0.5
  truncated[ind]="truncated"
  fit = mix(z=z.trunc,trunc=truncated,k=k,weights=weights)
  fit$sigma_SNR=sqrt(fit$sigma^2 - 1)          # stdev van SNR
  
  return(fit)
}

plot_mixture <- function(fit, z, weight) {
  omega=fit$omega[1]
  B1=2*pmix(-1.96,p=fit$p,m=fit$m,s=fit$sigma) # prob. |z|>1.96
  B2=1-B1

  df=data.frame(z=abs(z),weight)
  df$fz=2*(omega*(df$z<1.96) + (df$z>=1.96))*
    dmix(d$z,p=fit$p,m=fit$m,s=fit$sigma)/(B1 + omega*B2)
  
  df$fz2=2*dmix(df$z,p=fit$p,m=fit$m,s=fit$sigma)
  
  ggplot(df, aes(x = z)) +
    geom_histogram(aes(y = after_stat(density),
                       weight=weight), 
                   color="black",
                   fill="white", breaks=seq(0,10,0.2)) +
    geom_line(aes(x=z,y=fz)) +
    geom_line(aes(x=z,y=fz2),color="red") +
    xlab("absolute z-statistic") + ylab('') + 
    xlim(0,10) + theme_bw()
}

# Erik's function that used to do analysis within Rmd, I am now suggesting we
# precalculate this before running Rmd reports
fit_and_plot = function(z,truncated,k=4,weight){
  fit <- fit_mixture(z,truncated,k=4,weight)
  ggp <- plot_mixture(fit, z, weight)
  print(ggp)
  return(fit)
}
