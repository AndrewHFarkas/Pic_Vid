// The data fed into the model.
// Things we know, the results.
data {
  int nobs;
  int npar;
  int ncate;
  int nstim;
  real amp_mean_prior;
  real amp_sd_prior;
  array[nobs] int par;
  array[nobs] int cate;
  array[nobs] int stim;
  array[nobs] real amp;
  array[nobs] real arousal;
}


// If you want to transform your data in Stan.
// transformed data {

// }

// The parameters accepted by the model.
// Things we don't know, the posteriors we will estimate.
parameters {
  array[npar] real bpar;
  array[nstim] real bstim;
  real baro;
  real par_mean;
  real<lower = 0> par_sd;
  real<lower = 0> stim_sd;
  real<lower = 0> amp_sd;
}

// transformed parameters {
//   
// }

// The model to be estimated. 
// Priors and likelihood sampling models.
model {
  
  // declare priors
  // fixed uninformative
  par_mean ~ normal(amp_mean_prior, amp_sd_prior);
  par_sd ~ normal(0, amp_sd_prior);
  stim_sd ~ normal(0, amp_sd_prior);
  baro ~ normal(0, amp_sd_prior);
  amp_sd ~ normal(0, amp_sd_prior);
  
  
  
  // adaptive priors
  bpar ~ normal(par_mean, par_sd);
  bstim ~ normal(0, stim_sd);
  
  // Pre-calculate mu vector
  array[nobs] real mu;
  
  for (i in 1:nobs)
    mu[i] = bpar[par[i]] + bstim[stim[i]] + baro*arousal[i];
  
  amp ~ normal(mu, amp_sd); // Vectorized likelihood
}

generated quantities {
  real aro_r;
  real sd_aro = sd(arousal);
  real sd_amp = sd(amp);
  
  aro_r = (baro * sd_aro) / sd_amp;
  
  array[nobs] real log_lik;
  
  for (i in 1:nobs) {
    real mu = bpar[par[i]] + bstim[stim[i]] + baro*arousal[i]; // Only if needed elsewhere in generated quantities
    log_lik[i] = normal_lpdf(amp[i] | mu, amp_sd); // Use pre-calculated mu values
  }
}

