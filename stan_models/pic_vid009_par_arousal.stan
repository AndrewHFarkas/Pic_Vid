
// The data fed into the model.
// Things we know, the results.
data {
  int nobs;
  int npar;
  int ncate;
  int ntype;
  int nlpp;
  int nssvep;
  real picture_par_bar_mean_prior;
  real picture_par_bar_sd_prior;
  real picture_cate_bar_sd_prior;
  real picture_parxcate_sd_prior;
  // real picture_par_sd_mean_prior;
  // real picture_par_sd_sd_prior;
  real video_par_bar_mean_prior;
  real video_par_bar_sd_prior;
  real video_cate_bar_sd_prior;
  real video_parxcate_sd_prior;
  // real video_par_sd_mean_prior;
  // real video_par_sd_sd_prior;
  array[nobs] int par;
  array[nobs] int cate;
  array[nobs] int type;
  array[nobs] real amp;
  array[nobs] real arousal;
}


// If you want to transform your data in Stan.
transformed data {
  vector[nlpp] lpp_amp;
  vector[nssvep] ssvep_amp;
  int idx_lpp = 0;
  int idx_ssvep = 0;
  
  for (i in 1:nobs) {
    if (type[i] == 1) {
      idx_lpp += 1;
      lpp_amp[idx_lpp] = amp[i];
    }
    if (type[i] == 2) {
      idx_ssvep += 1;
      ssvep_amp[idx_ssvep] = amp[i];
    }
  }
}

// The parameters accepted by the model.
// Things we don't know, the posteriors we will estimate.
parameters {
  array[npar] real bpar_lpp;
  // array[ncate] real bcate_lpp;
  real baro_lpp;
  array[npar] real bpar_ssvep;
  // array[ncate] real bcate_ssvep;
  real baro_ssvep;
  real pic_par_mean;
  real vid_par_mean;
  real<lower = 0> pic_par_sd;
  // real<lower = 0> pic_cate_sd;
  real pic_par_df;
  // real pic_cate_df;
  real<lower = 0> vid_par_sd;
  // real<lower = 0> vid_cate_sd;
  real vid_par_df;
  // real vid_cate_df;
  real<lower = 0> lpp_sd;
  real<lower = 0> ssvep_sd;
}

// transformed parameters {
//   
// }

// The model to be estimated. 
// Priors and likelihood sampling models.
model {
  // declare variable, treated as data
  array[nlpp] real lpp_mu;
  array[nssvep] real ssvep_mu;
  
  // declare priors
  // fixed uninformative
  baro_lpp ~ normal(0, picture_par_bar_sd_prior);
  pic_par_mean ~ normal(picture_par_bar_mean_prior, picture_par_bar_sd_prior);
  pic_par_sd ~ normal(0, picture_par_bar_sd_prior);
  pic_par_df ~ gamma(100,2);
  lpp_sd ~ normal(0, picture_par_bar_sd_prior);
  
  baro_ssvep ~ normal(0, video_par_bar_sd_prior);
  vid_par_mean ~ normal(video_par_bar_mean_prior, video_par_bar_sd_prior);
  vid_par_sd ~ normal(0, video_par_bar_sd_prior);
  vid_par_df ~ gamma(100,2);
  ssvep_sd ~ normal(0, video_par_bar_sd_prior);
  
  // adaptive priors
  bpar_lpp ~ student_t(pic_par_df, pic_par_mean, pic_par_sd);
  bpar_ssvep ~ student_t(vid_par_df, vid_par_mean, vid_par_sd);
  
  int counter_lpp = 1;
  int counter_ssvep = 1;
  
  for (i in 1:nobs){
    if (type[i] == 1){
      lpp_mu[counter_lpp] = bpar_lpp[par[i]] + baro_lpp*arousal[i];
      counter_lpp += 1;
    }
    if (type[i] == 2){
      ssvep_mu[counter_ssvep] = bpar_ssvep[par[i]] + baro_ssvep*arousal[i];
      counter_ssvep += 1;
    }
  }
  lpp_amp ~ normal(lpp_mu, lpp_sd);
  ssvep_amp ~ normal(ssvep_mu, ssvep_sd);
}

// This section can generate log_likelihoods and posterior prediction distributions.
// Log_likelihoods can be used to compare models via cross-validation without 
// refitting the model. Posterior predictions can be used to find residuals.
generated quantities {
  // This works, but it isn't a good idea because it is going to make predictions
  // for the thousands of observations
  array[nobs] real log_lik;
  // array[nobs] real lpp_pred;
  // array[nobs] real ssvep_pred;
  //
  //
  
  real r_lpp;
  real sd_x = sd(arousal);
  real sd_lpp = sd(lpp_amp);
  
  r_lpp = (baro_lpp * sd_x) / sd_lpp;
  
  real r_ssvep;
  real sd_ssvep = sd(ssvep_amp);
  
  r_ssvep = baro_ssvep * (sd_x / sd_ssvep);
  
  for (i in 1:nobs){
    if (type[i] == 1){
      real lpp_mu_gq = bpar_lpp[par[i]] + baro_lpp*arousal[i];
      log_lik[i] = normal_lpdf(amp[i] | lpp_mu_gq, lpp_sd);
  //     lpp_pred[i] = normal_rng(lpp_mu_gq, lpp_sd);
    }
    if (type[i] == 2){
      real ssvep_mu_gq = bpar_ssvep[par[i]] + baro_ssvep*arousal[i];
      log_lik[i] = normal_lpdf(amp[i] | ssvep_mu_gq, ssvep_sd);
  //     ssvep_pred[i] = normal_rng(ssvep_mu_gq, ssvep_sd);
    }
  }
  // for (i in 1:nobs){
  //   if (type[i] == 1){
  //     real lpp_mu_gq = bpar_lpp[par[i]];
  //     lpp_log_lik[counter_lpp_gq] = normal_lpdf(amp[i] | lpp_mu_gq, lpp_sd);
  // //     lpp_pred[i] = normal_rng(lpp_mu_gq, lpp_sd);
  //     counter_lpp_gq += 1;
  //   }
  //   if (type[i] == 2){
  //     real ssvep_mu_gq = bpar_ssvep[par[i]];
  //     ssvep_log_lik[counter_lpp_gq] = normal_lpdf(amp[i] | ssvep_mu_gq, ssvep_sd);
  // //     ssvep_pred[i] = normal_rng(ssvep_mu_gq, ssvep_sd);
  //     counter_ssvep_gq += 1;
  //   }
  // }
}

