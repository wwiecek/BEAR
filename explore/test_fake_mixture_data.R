# helper to draw n zero-mean mixture normals
rmix_z <- function(n, p, sigma, k = length(p)) {
  if(length(p) == 1)
    return(rnorm(n, 0, sigma))
  comp <- sample.int(k, n, replace = TRUE, prob = p)
  rnorm(n, 0, sigma[comp])
}

gen_df <- function(
    p = c(0.3, 0.2, 0.1, 0.4),
    sigma = c(1.1, 1.8, 3.3, 4),
    omega = 0.5,
    n_study = 3000,
    lambda_extra # Poisson mean for add’l z’s
) {
  
  # simulation parameters
  p      <- p / sum(p)
  k      <- length(p) 
  
  study_list <- vector("list", n_study)         # collect per-study data
  published  <- 0                               # counter
  
  while (published < n_study) {
    
    ## draw a candidate study ----------------------------------------
    main_z   <- rmix_z(1, p, sigma)             # main test
    is_sig   <- abs(main_z) >= 1.96
    is_pub   <- if (is_sig) TRUE else runif(1) < omega
    if (!is_pub) next                           # discard, keep sampling
    
    ## published: add extra z’s --------------------------------------
    n_extra  <- if (runif(1) < 0.5) 0 else rpois(1, lambda_extra)
    z_vals   <- c(main_z, rmix_z(n_extra, p, sigma))
    
    study_list[[ published + 1 ]] <- data.frame(
      study_id = published + 1,
      z        = z_vals,
      operator = "=",                           # exact values only
      stringsAsFactors = FALSE
    )
    published <- published + 1
  }
  
  df <- do.call(rbind, study_list)
  study_sizes        <- table(df$study_id)
  df$weight          <- 1 / study_sizes[ as.character(df$study_id) ]
  
  df
  
}


# fit mixtures
sapply(c(0,0,0,0,0, 5,5,5,5,5), function(le) {
    df <- gen_df(lambda_extra = le)
    ff <- fit_mixture(z = df$z, 
                      operator = df$operator,
                      weight = df$weight)
    ff$omega[1]
})



df <- gen_df(lambda_extra = 0,
             p = rep(0.25, 4),
             sigma = rep(2, 4),
             n = 10000)
ff <- fit_mixture(z = df$z, 
                  operator = df$operator,
                  weight = df$weight, 
                  optimiser = "L-BFGS")
ff
plot_mixture(ff, df$z, df$weight)

ff <- fit_mixture(z = df$z, 
                  operator = df$operator,
                  weight = df$weight, 
                  optimiser = "Nelder-Mead")

loglik_op(theta = c(.43, .55, .01, 1.6, 2.1, 3.1, 3.7, .5),
                   z        = abs(df$z),
          
                   operator = df$operator,
                   k = 4, weights = df$weight)

# theta_true <- c(p[1:(k - 1)], sigma, omega)   # length 8
# 
# ll_v2 <- loglik_v2(theta = theta_true,
#                    z       = abs(df$z),
#                    truncated = rep(FALSE, nrow(df)),   # all “=”
#                    k = k, weights = df$weight)
# 
# ll_op <- loglik_op(theta = theta_true,
#                    z        = abs(df$z),
#                    operator = df$operator,
#                    k = k, weights = df$weight)
# 
# print(c(loglik_v2 = ll_v2, loglik_op = ll_op))

