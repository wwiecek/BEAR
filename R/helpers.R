## Functions for mixture of normals 
dmix = function(x,p,m,s){ # density of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) dnorm(x,mean=m,sd=s)))
}

rmix = function(n,p,m,s){    # sample from a normal mixture
  d=rmultinom(n,1,p)
  rnorm(n,m%*%d,s%*%d)
}

pmix = function(x,p,m,s){ # cdf of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) pnorm(x,mean=m,sd=s)))
}

qfun = function(q,p,m,s){   # quantile function scalar q
  uniroot(function(x) pmix(x,p,m,s)-q, interval=c(-20,20))$root
}
qmix = function(q,p,m,s){   # quantile function vector q
  sapply(q, function(q) qfun(q,p=p,m=m,s=s) )
}

## Functions for mixture of half-normals
dmixabs = function(x,p,m,s){ # density of normal mixture (vector x)
  drop(p %*% sapply(x, function(x) (dnorm(-x,mean=m,sd=s) + dnorm(x,mean=m,sd=s))))
}

pmixabs = function(x,p,m,s){ # cumulative distr of |x|
  drop(p %*% sapply(x, function(x) pnorm(x,mean=m,sd=s) - pnorm(-x,mean=m,sd=s)))
}

qfunabs = function(q,p,m,s){ # quantile function scalar q
  uniroot(function(x) pmixabs(x,p,m,s)-q, interval=c(0,20))$root
}
qmixabs = function(q,p,m,s){ # quantile function vector q
  sapply(q, function(q) qfunabs(q,p=p,m=m,s=s) )
}

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
