functions {
  real pmix_fold(real x, vector p, vector mu, vector sigma) {
    real val = 0;
    int K = num_elements(p);
    for (k in 1:K) {
      real c_hi = normal_cdf( x,  mu[k], sigma[k]);
      real c_lo = normal_cdf(-x,  mu[k], sigma[k]);
      val += p[k] * (c_hi - c_lo);              // Φ(x) − Φ(−x)
    }
    return val;                                 // NOT on log-scale
  }
  
  /*  log-PDF   log f(|Z| = x)   */
  real dmix_fold_log(real x, vector p, vector mu, vector sigma) {
    vector[num_elements(p)] log_comp;
    for (k in 1:num_elements(p)) {
      real lp1 = normal_lpdf( x |  mu[k], sigma[k]);
      real lp2 = normal_lpdf(-x |  mu[k], sigma[k]);
      log_comp[k] = log(p[k]) + log_sum_exp(lp1, lp2);
    }
    return log_sum_exp(log_comp);               // mixture log-density
  }
}


data {
  int<lower=1> N;                  // number of obs
  int<lower=1> K;                  // mixture components
  vector<lower=0>[N] z;            // |z| values (capped, ≥0)
  int<lower=0,upper=1> truncated[N];// 1 = truncated, 0 = exact
  int<lower=0,upper=1> is_high[N]; // 1 = |z| ≥ 1.96
  vector<lower=0>[N] w;            // inverse‑study‑size weights
}

parameters {
  simplex[K] p;
  ordered[K] sigma_raw;
  vector<lower=0>[K] mu;
  real<lower=0> omega;
}
transformed parameters {
  vector<lower=1, upper=20>[K] sigma = 1 + exp(sigma_raw);   // ≥1 as before
  real B1     = 1 - pmix_fold(1.96, p, mu, sigma);   // P(|Z| ≥ 1.96)
  real B2     = 1 - B1;                              // P(|Z| < 1.96)
  real l_norm = log(B1 + omega * B2);                // log normaliser
}

model {
  /* ----- priors (choose your own) ----- */
  mu ~ normal(0,3);
  p     ~ dirichlet(rep_vector(1.0, K));
  sigma_raw ~ student_t(3, 0, 4); 
  omega ~ uniform(.05, .95); //should easily be wide enough!
  
  /* ----- likelihood ----- */
  for (n in 1:N) {
    real lp;
    if (is_high[n]==0 && truncated[n]==1)          // |z|<1.96 & truncated high
    lp = log(omega) + log( pmix_fold(z[n], p, mu, sigma) );
    else if (is_high[n]==0 && truncated[n]==0)     // |z|<1.96 & exact
    lp = log(omega) +       dmix_fold_log(z[n], p, mu, sigma);
    else if (is_high[n]==1 && truncated[n]==0)     // |z|≥1.96 & exact
    lp =                   dmix_fold_log(z[n], p, mu, sigma);
    else                                           // |z|≥1.96 & truncated low
    lp = log1m( pmix_fold(z[n], p, mu, sigma) );
    
    target += w[n] * (lp - l_norm);
  }
  
}

generated quantities {
  vector[K] sigma_SNR;        // same diagnostic you compute in R
  for (k in 1:K)
  sigma_SNR[k] = sqrt(square(sigma[k]) - 1);
}

