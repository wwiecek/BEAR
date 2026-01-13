functions {
  // log‑pdf of a half‑Normal  N⁺(0,σ²)  for x ≥ 0
  real halfnorm_den(real x, real sigma) {
    return (x < 0) ? negative_infinity()
                   : log(2) + normal_lpdf(x | 0, sigma);
  }

  // CDF of the same half‑Normal
  real halfnorm_cdf(real x, real sigma) {
    return (x < 0) ? 0.0 : 2 * Phi(x / sigma) - 1;
  }

  // mixture log‑pdf  (p must already sum to 1)
  real mix_halfnorm_den(real x, vector p, vector sigma) {
    int K = num_elements(p);
    vector[K] lps;
    for (k in 1:K)
      lps[k] = log(p[k]) + halfnorm_den(x, sigma[k]);
    return log_sum_exp(lps);
  }

  // mixture CDF
  real mix_halfnorm_cdf(real x, vector p, vector sigma) {
    real c = 0;
    for (k in 1:num_elements(p))
      c += p[k] * halfnorm_cdf(x, sigma[k]);
    return c;
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
  ordered[K] sigma_raw;           // strictly increasing
  real<lower=0> omega;
}
transformed parameters {
  vector<lower=1, upper=20>[K] sigma = 1 + exp(sigma_raw);   // ≥1 as before
  real B1     = 1 - mix_halfnorm_cdf(1.96, p, sigma);   // P(|Z| ≥ 1.96)
  real B2     = 1 - B1;                                 // P(|Z| < 1.96)
  real l_norm = log(B1 + omega * B2);                   // log normaliser
}

model {
  /* ----- priors (choose your own) ----- */
  p     ~ dirichlet(rep_vector(1.0, K));
  sigma_raw ~ student_t(3, 0, 4); 
  omega ~ uniform(.05, .95); //should easily be wide enough!

  /* ----- likelihood ----- */
  for (n in 1:N) {
    real lp;
    if (is_high[n]==0 && truncated[n]==1)               // |z|<1.96 & truncated above
      lp = log(omega) + log(mix_halfnorm_cdf(z[n], p, sigma));
    else if (is_high[n]==0 && truncated[n]==0)          // |z|<1.96 & observed exactly
      lp = log(omega) +        mix_halfnorm_den(z[n], p, sigma);
    else if (is_high[n]==1 && truncated[n]==0)          // |z|≥1.96 & observed exactly
      lp =                 mix_halfnorm_den(z[n], p, sigma);
    else                                              // |z|≥1.96 & truncated below
      lp = log1m(mix_halfnorm_cdf(z[n], p, sigma));   // log(1‒CDF)

    target += w[n] * (lp - l_norm);
  }
}

generated quantities {
  vector[K] sigma_SNR;        // same diagnostic you compute in R
  for (k in 1:K)
    sigma_SNR[k] = sqrt(square(sigma[k]) - 1);
}

