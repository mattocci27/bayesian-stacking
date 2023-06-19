data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

transformed parameters {
  vector[N] mu = alpha + beta * x;
}

model {
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
}
