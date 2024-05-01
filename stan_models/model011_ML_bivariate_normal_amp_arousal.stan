data {
  int<lower=0> nobs; // Number of observations
  int<lower=0> npar; // Number of participants
  array[nobs] int par; // Participant index
  array[nobs] real amp; // Amplitude measurements
  array[nobs] real arousal; // Arousal measurements
  real amp_mean_prior;
  real amp_sd_prior;
}

parameters {
  real mu_amp;
  real<lower=0> par_amp_distribution_sd;
  real<lower=0> mu_par_sd_amp;
  real<lower=0> par_sd_amp_distribution_sd;
  
  // real<lower=1, upper=9> mu_aro;
  real<lower=0, upper=1> mu_aro_raw;
  real<lower=0> par_aro_distribution_sd;
  real<lower=0> mu_par_sd_aro;
  real<lower=0> par_sd_aro_distribution_sd;
  
  real mu_cov_amp_aro;
  real<lower=0> par_cov_amp_aro_distribution_sd;
  
  array[npar] real par_mu_amp;
  array[npar] real<lower=0> par_sd_amp;
  
  array[npar] real <lower=1, upper=9> par_mu_aro;
  array[npar] real <lower=1, upper=9> par_sd_aro;
  
  array[npar] real par_cov_amp_aro;
  
}

transformed parameters {
  real mu_aro = (8 * mu_aro_raw) + 1;  // Transform to range between 1 and 9
  // real mu_aro = 1 + 8 * mu_aro_raw;  // Transform to range between 1 and 9
  
  array[npar] matrix[2,2] par_cov_mat;        // Covariance matrix for each participant
  array[npar] cholesky_factor_cov[2,2] L_par_cov;  // Cholesky factor of the covariance matrix
  
  // Assemble the covariance matrix and compute its Cholesky decomposition
  for (j in 1:npar) {
    par_cov_mat[j][1,1] = par_sd_amp[j] * par_sd_amp[j];
    par_cov_mat[j][1,2] = par_cov_amp_aro[j];
    par_cov_mat[j][2,1] = par_cov_amp_aro[j];
    par_cov_mat[j][2,2] = par_sd_aro[j] * par_sd_aro[j];

    L_par_cov[j] = cholesky_decompose(par_cov_mat[j]);
  }
}

model {
  
  // Fixed uninformative priors
  mu_amp ~ normal(amp_mean_prior, amp_sd_prior);
  par_amp_distribution_sd ~ normal(0, amp_sd_prior);
  mu_par_sd_amp ~ normal(0, amp_sd_prior);
  par_sd_amp_distribution_sd ~ normal(0, amp_sd_prior);
  
  mu_aro_raw ~ beta(1.1, 1.1);   // Sampling the raw parameter from a Beta distribution, transformed above
  par_aro_distribution_sd ~ normal(0, 4);
  mu_par_sd_aro ~ normal(0, 4);
  par_sd_aro_distribution_sd ~ normal(0, 4);
  
  mu_cov_amp_aro ~ normal(0, amp_sd_prior);
  par_cov_amp_aro_distribution_sd ~ normal(0, amp_sd_prior);
  
  // Regularizing multi-level priors
  par_mu_amp ~ normal(mu_amp, par_amp_distribution_sd);
  par_sd_amp ~ normal(mu_par_sd_amp, par_sd_amp_distribution_sd);
  par_mu_aro ~ normal(mu_aro, par_aro_distribution_sd);
  par_sd_aro ~ normal(mu_par_sd_aro, par_sd_aro_distribution_sd);
  par_cov_amp_aro ~ normal(mu_cov_amp_aro, par_cov_amp_aro_distribution_sd);
  
  
  // Likelihood
  for (i in 1:nobs) {
    vector[2] outcomes = [amp[i], arousal[i]]';
    vector[2] means = [par_mu_amp[par[i]], par_mu_aro[par[i]]]';
    outcomes ~ multi_normal_cholesky(means, L_par_cov[par[i]]);
    // outcomes ~ multi_normal(means, par_cov_mat[par[i]]);
    // [amp[i], arousal[i]]' ~ multi_normal_cholesky([par_mu_amp[par[i]] par_mu_aro[par[i]]], L_par_cov[par[i]]);
  }
  
}

generated quantities {
  
  vector[npar] par_amp_aro_cor;
  
  for (j in 1:npar) {
    par_amp_aro_cor[j] = (par_cov_amp_aro[j]) / (par_sd_amp[j] * par_sd_aro[j]);
  }

  
  
  array[nobs] real mu_amp_given_aro;  // Log likelihood for amplitude predictions
  
  array[nobs] real log_lik;  // Log likelihood for amplitude predictions
  
  for (i in 1:nobs) {
    // Decompose the covariance matrix to get the conditional parameters
    mu_amp_given_aro[i] = par_mu_amp[par[i]] + 
                          (par_cov_amp_aro[par[i]] / 
                          (par_sd_aro[par[i]] * par_sd_aro[par[i]])) * 
                          (arousal[i] - par_mu_aro[par[i]]);
                            
    real var_amp_given_aro = par_sd_amp[par[i]] * par_sd_amp[par[i]] - 
                             (par_cov_amp_aro[par[i]] * 
                             par_cov_amp_aro[par[i]]) / 
                             (par_sd_aro[par[i]] * par_sd_aro[par[i]]);

    // Calculate log likelihood for amplitude given observed arousal
    log_lik[i] = normal_lpdf(amp[i] | mu_amp_given_aro[i], sqrt(var_amp_given_aro));
  }
}
