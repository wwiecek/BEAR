functions {
/* ------------  helpers for mixtures of |Z|  ----------------- */

/*  CDF  P(|Z| ≤ x)   */
real pmixabs(real x,
             vector theta,
             vector mu,
             vector sigma,
             int    K) {
  real val = 0;
  for (k in 1:K) {
    real cdf_hi = normal_cdf( x,  mu[k], sigma[k]);
    real cdf_lo = normal_cdf(-x,  mu[k], sigma[k]);
    val += theta[k] * (cdf_hi - cdf_lo);           // Φ(x) − Φ(−x)
  }
  return val;
}

/*  PDF  f(|Z| = x)   */
real dmixabs(real x,
             vector theta,
             vector mu,
             vector sigma,
             int    K) {
  vector[K] log_comp;
  for (k in 1:K) {
    // sum of the two mirror densities:  φ(x|μ,σ)+φ(−x|μ,σ)
    real log1 = normal_lpdf( x | mu[k], sigma[k]);
    real log2 = normal_lpdf(-x | mu[k], sigma[k]);
    log_comp[k] = log(theta[k]) + log_sum_exp(log1, log2);
  }
  return exp(log_sum_exp(log_comp));
}
}

data {
  int<lower=1> N;                       // studies
  int<lower=1> K;                       // mixture components
  vector<lower=0>[N] z;                 // absolute z-scores
  int<lower=0,upper=1> truncated[N];    // 1 = upper/lower-censored
  vector<lower=0>[N] w;                 // weights
  real<lower=0> thresh;                 // significance cut (e.g. 1.96)
}

parameters {
  simplex[K]            theta;          // mixture probabilities
  vector<lower=0>[K]    mu;             // component means (ordered ↔ identifiability)
  positive_ordered[K]   sigma;          // component s.d.s  (σ₁<σ₂<…)
  real<lower=0,upper=1> omega;          // publication-bias factor
}

transformed parameters {
  /* constants that only depend on parameters */
  real pmix_thr  = pmixabs(thresh, theta, mu, sigma, K);   // P(|Z|≤thr)
  real B1        = 1 - pmix_thr;                           // P(|Z|>thr)
  real norm_cnst = B1 + omega * pmix_thr;                  // denominator
}

model {
  /* ---------- priors (weakly-informative) ---------- */
  mu    ~ normal(0, 3);            // |μ| rarely > 6 under z ≤ 10
  sigma ~ lognormal(0, 0.6);       // σ roughly 0.3 – 5.5
  omega ~ beta(2, 2);              // 80 % mass on 0.1–0.9
  // theta has Dirichlet(1,…,1) from the simplex declaration

  /* ---------- likelihood (four cases) -------------- */
  for (n in 1:N) {
    real z_n   = z[n];
    real p_mix = pmixabs(z_n, theta, mu, sigma, K);
    real d_mix = dmixabs(z_n, theta, mu, sigma, K);
    real numer;

    if (z_n < thresh) {
      numer = (truncated[n]==1)
                ? omega * p_mix          // case 1: censored low
                : omega * d_mix;         // case 2: exact low
    } else {
      numer = (truncated[n]==0)
                ? d_mix                  // case 3: exact high
                : 1 - p_mix;             // case 4: censored high
    }
    target += w[n] * (log(numer) - log(norm_cnst));
  }
}
