data {
  int<lower=0> M;        // subsets
  int<lower=0> N;        // replicates
  int p[M, N];           // numerator
  int q[M, N];           // denominator
  real b_alpha_prior;    // prior alpha for Beta distribution
  real b_beta_prior;     // prior beta for Beta distribution
  real n_mean_prior;     // prior mean for Normal distribution
  real n_variance_prior; // prior variance for Normal distribution
}

parameters {
  real<lower=1> sigma;             // estimate variance parameter to capture replicate variance
  real<lower=0, upper=1.0> rho[M]; // estimate a proportion for each subset
}

model {
  sigma ~ normal(n_mean_prior, n_variance_prior);
  for(i in 1:M) {
    for(j in 1:N) {
      rho[i] ~ beta(b_alpha_prior, b_beta_prior);
      p[i, j] ~ neg_binomial_2(rho[i] * q[i, j], sigma);
    }
  }
}
