
data {
  int N_time;
  int Lag_temp;
  int Lag_light;
  int Lag_SIG5;
  int max_Lag;
  int N_SepSun[N_time];
  int N_SepShade[N_time];
  int sumN_SepSun;
  int sumN_SepShade;
  vector[N_time] temp_SepSun;
  vector[N_time] temp_SepShade;
  vector[N_time] light_SepSun;
  vector[N_time] light_SepShade;
  vector[N_time] SIG5_SepSun;
  vector[N_time] SIG5_SepShade;
  vector[sumN_SepSun] Y_SepSun;
  vector[sumN_SepShade] Y_SepShade;
}


parameters {
  vector[N_time] mu;
  vector[N_time] b_temp;
  vector[N_time] b_light;
  vector[N_time] b_SIG5;
  real<lower=0> s_mu;
  real<lower=0> s_b_temp;
  real<lower=0> s_b_light;
  real<lower=0> s_b_SIG5;
  real<lower=0> s_Y;
}


transformed parameters {
  vector[N_time] alpha_SepSun;
  vector[N_time] alpha_SepShade;
  //alpha_SepSun = mu + b_temp * temp_SepSun + b_light * light_SepSun;
  if (max_Lag >= 1) {
    alpha_SepSun[1] = mean(Y_SepSun[1:N_SepSun[1]]);
    alpha_SepShade[1] = mean(Y_SepShade[1:N_SepShade[1]]);
  }
  if (max_Lag >= 2) {
    alpha_SepSun[2] = mean(Y_SepSun[(N_SepSun[1]+1):(N_SepSun[1]+N_SepSun[2])]);
    alpha_SepShade[2] = mean(Y_SepShade[(N_SepShade[1]+1):(N_SepShade[1]+N_SepShade[2])]);
  }
  if (max_Lag >= 3) {
    alpha_SepSun[3] = mean(Y_SepSun[(N_SepSun[1]+N_SepSun[2]+1):(N_SepSun[1]+N_SepSun[2]+N_SepSun[3])]);
    alpha_SepShade[3] = mean(Y_SepShade[(N_SepShade[1]+N_SepShade[2]+1):(N_SepShade[1]+N_SepShade[2]+N_SepShade[3])]);
  }
  if (max_Lag >= 4) {
    alpha_SepSun[4] = mean(Y_SepSun[(N_SepSun[1]+N_SepSun[2]+N_SepSun[3]+1):(N_SepSun[1]+N_SepSun[2]+N_SepSun[3]+N_SepSun[4])]);
    alpha_SepShade[4] = mean(Y_SepShade[(N_SepShade[1]+N_SepShade[2]+N_SepShade[3]+1):(N_SepShade[1]+N_SepShade[2]+N_SepShade[3]+N_SepShade[4])]);
  }
  if (max_Lag >= 5) {
    alpha_SepSun[5] = mean(Y_SepSun[(N_SepSun[1]+N_SepSun[2]+N_SepSun[3]+N_SepSun[4]+1):(N_SepSun[1]+N_SepSun[2]+N_SepSun[3]+N_SepSun[4]+N_SepSun[5])]);
    alpha_SepShade[5] = mean(Y_SepShade[(N_SepShade[1]+N_SepShade[2]+N_SepShade[3]+N_SepShade[4]+1):(N_SepShade[1]+N_SepShade[2]+N_SepShade[3]+N_SepShade[4]+N_SepShade[5])]);
  }

  for (t in (max_Lag+1):N_time) {
    alpha_SepSun[t] = mu[t] + b_temp[t] * temp_SepSun[t-Lag_temp] + b_light[t] * light_SepSun[t-Lag_light] + b_SIG5[t] * SIG5_SepSun[t-Lag_SIG5];
  }
  for (t in (max_Lag+1):N_time) {
    alpha_SepShade[t] = mu[t] + b_temp[t] * temp_SepShade[t-Lag_temp] + b_light[t] * light_SepShade[t-Lag_light] + b_SIG5[t] * SIG5_SepShade[t-Lag_SIG5];
  }
}


model {
  
  // State equation of mu
  if (max_Lag == 0) {
    for (t in 2:N_time) {
      mu[t] ~ normal(mu[t-1], s_mu);
      b_temp[t] ~ normal(b_temp[t-1], s_b_temp);
      b_light[t] ~ normal(b_light[t-1], s_b_light);
      b_SIG5[t] ~ normal(b_SIG5[t-1], s_b_SIG5);
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      mu[t] ~ normal(mu[t-1], s_mu);
      b_temp[t] ~ normal(b_temp[t-1], s_b_temp);
      b_light[t] ~ normal(b_light[t-1], s_b_light);
      b_SIG5[t] ~ normal(b_SIG5[t-1], s_b_SIG5);
    }
  }
  
  // Observation equation of Y_SepSun
  if (max_Lag == 0) {
    for (n in 1:N_SepSun[1]) {                   // N_SepSun=c(6,6,6,...)
      target += normal_lpdf(Y_SepSun[n] | alpha_SepSun[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_SepSun[t]) {
        target += normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_SepSun[t]) {
        target += normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
      }
    }
  }
  
  // Observation equation of Y_SepShade
  if (max_Lag == 0) {
    for (n in 1:N_SepShade[1]) {                   // N_SepShade=c(6,6,6,...)
      target += normal_lpdf(Y_SepShade[n] | alpha_SepShade[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_SepShade[t]) {
        target += normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_SepShade[t]) {
        target += normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
      }
    }
  }
  
}


generated quantities {
  vector[sumN_SepSun] log_lik_SepSun;
  vector[sumN_SepShade] log_lik_SepShade;
  
  // Log likelihood of Y_SepSun
  if (max_Lag == 0) {
    for (n in 1:N_SepSun[1]) {                   // N_SepSun=c(6,6,6,...)
      log_lik_SepSun[n] = normal_lpdf(Y_SepSun[n] | alpha_SepSun[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_SepSun[t]) {
        log_lik_SepSun[sum(N_SepSun[1:(t-1)])+n] = normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_SepSun[t]) {
        log_lik_SepSun[sum(N_SepSun[1:(t-1)])+n] = normal_lpdf(Y_SepSun[sum(N_SepSun[1:(t-1)])+n] | alpha_SepSun[t], s_Y);
      }
    }
  }
  
  // Log likelihood of Y_SepShade
  if (max_Lag == 0) {
    for (n in 1:N_SepShade[1]) {                   // N_SepShade=c(6,6,6,...)
      log_lik_SepShade[n] = normal_lpdf(Y_SepShade[n] | alpha_SepShade[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_SepShade[t]) {
        log_lik_SepShade[sum(N_SepShade[1:(t-1)])+n] = normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_SepShade[t]) {
        log_lik_SepShade[sum(N_SepShade[1:(t-1)])+n] = normal_lpdf(Y_SepShade[sum(N_SepShade[1:(t-1)])+n] | alpha_SepShade[t], s_Y);
      }
    }
  }
  
}
  