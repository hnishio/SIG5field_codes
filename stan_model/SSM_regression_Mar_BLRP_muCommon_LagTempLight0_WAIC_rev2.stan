
data {
  int N_time;
  int Lag_temp;
  int Lag_light;
  int Lag_SIG5;
  int max_Lag;
  int N_MarSun[N_time];
  int N_MarShade[N_time];
  int sumN_MarSun;
  int sumN_MarShade;
  vector[N_time] temp_MarSun;
  vector[N_time] temp_MarShade;
  vector[N_time] light_MarSun;
  vector[N_time] light_MarShade;
  vector[N_time] SIG5_MarSun;
  vector[N_time] SIG5_MarShade;
  vector[sumN_MarSun] Y_MarSun;
  vector[sumN_MarShade] Y_MarShade;
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
  vector[N_time] alpha_MarSun;
  vector[N_time] alpha_MarShade;
  //alpha_MarSun = mu + b_temp * temp_MarSun + b_light * light_MarSun;
  if (max_Lag >= 1) {
    alpha_MarSun[1] = mean(Y_MarSun[1:N_MarSun[1]]);
    alpha_MarShade[1] = mean(Y_MarShade[1:N_MarShade[1]]);
  }
  if (max_Lag >= 2) {
    alpha_MarSun[2] = mean(Y_MarSun[(N_MarSun[1]+1):(N_MarSun[1]+N_MarSun[2])]);
    alpha_MarShade[2] = mean(Y_MarShade[(N_MarShade[1]+1):(N_MarShade[1]+N_MarShade[2])]);
  }
  if (max_Lag >= 3) {
    alpha_MarSun[3] = mean(Y_MarSun[(N_MarSun[1]+N_MarSun[2]+1):(N_MarSun[1]+N_MarSun[2]+N_MarSun[3])]);
    alpha_MarShade[3] = mean(Y_MarShade[(N_MarShade[1]+N_MarShade[2]+1):(N_MarShade[1]+N_MarShade[2]+N_MarShade[3])]);
  }
  if (max_Lag >= 4) {
    alpha_MarSun[4] = mean(Y_MarSun[(N_MarSun[1]+N_MarSun[2]+N_MarSun[3]+1):(N_MarSun[1]+N_MarSun[2]+N_MarSun[3]+N_MarSun[4])]);
    alpha_MarShade[4] = mean(Y_MarShade[(N_MarShade[1]+N_MarShade[2]+N_MarShade[3]+1):(N_MarShade[1]+N_MarShade[2]+N_MarShade[3]+N_MarShade[4])]);
  }
  if (max_Lag >= 5) {
    alpha_MarSun[5] = mean(Y_MarSun[(N_MarSun[1]+N_MarSun[2]+N_MarSun[3]+N_MarSun[4]+1):(N_MarSun[1]+N_MarSun[2]+N_MarSun[3]+N_MarSun[4]+N_MarSun[5])]);
    alpha_MarShade[5] = mean(Y_MarShade[(N_MarShade[1]+N_MarShade[2]+N_MarShade[3]+N_MarShade[4]+1):(N_MarShade[1]+N_MarShade[2]+N_MarShade[3]+N_MarShade[4]+N_MarShade[5])]);
  }

  for (t in (max_Lag+1):N_time) {
    alpha_MarSun[t] = mu[t] + b_temp[t] * temp_MarSun[t-Lag_temp] + b_light[t] * light_MarSun[t-Lag_light] + b_SIG5[t] * SIG5_MarSun[t-Lag_SIG5];
  }
  for (t in (max_Lag+1):N_time) {
    alpha_MarShade[t] = mu[t] + b_temp[t] * temp_MarShade[t-Lag_temp] + b_light[t] * light_MarShade[t-Lag_light] + b_SIG5[t] * SIG5_MarShade[t-Lag_SIG5];
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
  
  // Observation equation of Y_MarSun
  if (max_Lag == 0) {
    for (n in 1:N_MarSun[1]) {                   // N_MarSun=c(6,6,6,...)
      target += normal_lpdf(Y_MarSun[n] | alpha_MarSun[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_MarSun[t]) {
        target += normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_MarSun[t]) {
        target += normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
      }
    }
  }
  
  // Observation equation of Y_MarShade
  if (max_Lag == 0) {
    for (n in 1:N_MarShade[1]) {                   // N_MarShade=c(6,6,6,...)
      target += normal_lpdf(Y_MarShade[n] | alpha_MarShade[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_MarShade[t]) {
        target += normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_MarShade[t]) {
        target += normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
      }
    }
  }
  
}


generated quantities {
  vector[sumN_MarSun] log_lik_MarSun;
  vector[sumN_MarShade] log_lik_MarShade;
  
  // Log likelihood of Y_MarSun
  if (max_Lag == 0) {
    for (n in 1:N_MarSun[1]) {                   // N_MarSun=c(6,6,6,...)
      log_lik_MarSun[n] = normal_lpdf(Y_MarSun[n] | alpha_MarSun[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_MarSun[t]) {
        log_lik_MarSun[sum(N_MarSun[1:(t-1)])+n] = normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_MarSun[t]) {
        log_lik_MarSun[sum(N_MarSun[1:(t-1)])+n] = normal_lpdf(Y_MarSun[sum(N_MarSun[1:(t-1)])+n] | alpha_MarSun[t], s_Y);
      }
    }
  }
  
  // Log likelihood of Y_MarShade
  if (max_Lag == 0) {
    for (n in 1:N_MarShade[1]) {                   // N_MarShade=c(6,6,6,...)
      log_lik_MarShade[n] = normal_lpdf(Y_MarShade[n] | alpha_MarShade[1], s_Y);
    }
    for (t in 2:N_time) {
      for (n in 1:N_MarShade[t]) {
        log_lik_MarShade[sum(N_MarShade[1:(t-1)])+n] = normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
      }
    }
  } else {
    for (t in (max_Lag+1):N_time) {
      for (n in 1:N_MarShade[t]) {
        log_lik_MarShade[sum(N_MarShade[1:(t-1)])+n] = normal_lpdf(Y_MarShade[sum(N_MarShade[1:(t-1)])+n] | alpha_MarShade[t], s_Y);
      }
    }
  }
  
}
  