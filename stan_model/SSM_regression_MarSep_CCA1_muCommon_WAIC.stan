
data {
  int N_time;
  int Lag_temp;
  int Lag_light;
  int max_Lag;
  int N_MarSun[N_time];
  int N_MarShade[N_time];
  int N_SepSun[N_time];
  int N_SepShade[N_time];
  int sumN_MarSun;
  int sumN_MarShade;
  int sumN_SepSun;
  int sumN_SepShade;
  vector[N_time] temp_MarSun;
  vector[N_time] temp_MarShade;
  vector[N_time] temp_SepSun;
  vector[N_time] temp_SepShade;
  vector[N_time] light_MarSun;
  vector[N_time] light_MarShade;
  vector[N_time] light_SepSun;
  vector[N_time] light_SepShade;
  vector[sumN_MarSun] Y_MarSun;
  vector[sumN_MarShade] Y_MarShade;
  vector[sumN_SepSun] Y_SepSun;
  vector[sumN_SepShade] Y_SepShade;
}


parameters {
  vector[N_time] mu;
  vector[N_time] b_temp;
  vector[N_time] b_light;
  real<lower=0> s_mu;
  real<lower=0> s_b_temp;
  real<lower=0> s_b_light;
  real<lower=0> s_Y;
}


transformed parameters {
  vector[N_time] alpha_MarSun;
  vector[N_time] alpha_MarShade;
  vector[N_time] alpha_SepSun;
  vector[N_time] alpha_SepShade;
  //alpha_MarSun = mu + b_temp * temp_MarSun + b_light * light_MarSun;
  if (max_Lag >= 1) {
    alpha_MarSun[1] = mean(Y_MarSun[1:N_MarSun[1]]);
    alpha_MarShade[1] = mean(Y_MarShade[1:N_MarShade[1]]);
    alpha_SepSun[1] = mean(Y_SepSun[1:N_SepSun[1]]);
    alpha_SepShade[1] = mean(Y_SepShade[1:N_SepShade[1]]);
  }
  if (max_Lag == 2) {
    alpha_MarSun[2] = mean(Y_MarSun[(N_MarSun[1]+1):(N_MarSun[1]+N_MarSun[2])]);
    alpha_MarShade[2] = mean(Y_MarShade[(N_MarShade[1]+1):(N_MarShade[1]+N_MarShade[2])]);
    alpha_SepSun[2] = mean(Y_SepSun[(N_SepSun[1]+1):(N_SepSun[1]+N_SepSun[2])]);
    alpha_SepShade[2] = mean(Y_SepShade[(N_SepShade[1]+1):(N_SepShade[1]+N_SepShade[2])]);
  }
  for (t in (max_Lag+1):N_time) {
    alpha_MarSun[t] = mu[t] + b_temp[t] * temp_MarSun[t-Lag_temp] + b_light[t] * light_MarSun[t-Lag_light];
  }
  for (t in (max_Lag+1):N_time) {
    alpha_MarShade[t] = mu[t] + b_temp[t] * temp_MarShade[t-Lag_temp] + b_light[t] * light_MarShade[t-Lag_light];
  }
  for (t in (max_Lag+1):N_time) {
    alpha_SepSun[t] = mu[t] + b_temp[t] * temp_SepSun[t-Lag_temp] + b_light[t] * light_SepSun[t-Lag_light];
  }
  for (t in (max_Lag+1):N_time) {
    alpha_SepShade[t] = mu[t] + b_temp[t] * temp_SepShade[t-Lag_temp] + b_light[t] * light_SepShade[t-Lag_light];
  }
}


model {
  
  // State equation of mu
  for (t in 2:N_time) {
    mu[t] ~ normal(mu[t-1], s_mu);
    b_temp[t] ~ normal(b_temp[t-1], s_b_temp);
    b_light[t] ~ normal(b_light[t-1], s_b_light);
  }
  
  // Observation equation of Y_MarSun
  for (n in 1:N_MarSun[1]) {                   // N_MarSun=c(6,6,6,...)
    target += normal_lpdf(Y_MarSun[n] | alpha_MarSun[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_MarSun[t]) {
      target += normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
    }
  }
  
  // Observation equation of Y_MarShade
  for (n in 1:N_MarShade[1]) {                   // N_MarShade=c(6,6,6,...)
    target += normal_lpdf(Y_MarShade[n] | alpha_MarShade[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_MarShade[t]) {
      target += normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
    }
  }

  // Observation equation of Y_SepSun
  for (n in 1:N_SepSun[1]) {                   // N_SepSun=c(6,6,6,...)
    target += normal_lpdf(Y_SepSun[n] | alpha_SepSun[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_SepSun[t]) {
      target += normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
    }
  }
  
  // Observation equation of Y_SepShade
  for (n in 1:N_SepShade[1]) {                   // N_SepShade=c(6,6,6,...)
    target += normal_lpdf(Y_SepShade[n] | alpha_SepShade[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_SepShade[t]) {
      target += normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
    }
  }

}


generated quantities {
  vector[sumN_MarSun] log_lik_MarSun;
  vector[sumN_MarShade] log_lik_MarShade;
  vector[sumN_SepSun] log_lik_SepSun;
  vector[sumN_SepShade] log_lik_SepShade;
  
  // Log likelihood
  for (n in 1:N_MarSun[1]) {                   // N_MarSun=c(6,6,6,...)
    log_lik_MarSun[n] = normal_lpdf(Y_MarSun[n] | alpha_MarSun[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_MarSun[t]) {
      log_lik_MarSun[sum(N_MarSun[1:(t-1)])+n] = normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
    }
  }
  
  for (n in 1:N_MarShade[1]) {                   // N_MarShade=c(6,6,6,...)
    log_lik_MarShade[n] = normal_lpdf(Y_MarShade[n] | alpha_MarShade[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_MarShade[t]) {
      log_lik_MarShade[sum(N_MarShade[1:(t-1)])+n] = normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
    }
  }

  for (n in 1:N_SepSun[1]) {                   // N_SepSun=c(6,6,6,...)
    log_lik_SepSun[n] = normal_lpdf(Y_SepSun[n] | alpha_SepSun[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_SepSun[t]) {
      log_lik_SepSun[sum(N_SepSun[1:(t-1)])+n] = normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
    }
  }

  for (n in 1:N_SepShade[1]) {                   // N_SepShade=c(6,6,6,...)
    log_lik_SepShade[n] = normal_lpdf(Y_SepShade[n] | alpha_SepShade[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N_SepShade[t]) {
      log_lik_SepShade[sum(N_SepShade[1:(t-1)])+n] = normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
    }
  }
  
}
