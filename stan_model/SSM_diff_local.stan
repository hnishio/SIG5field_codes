
data {
  int N_time;
  int N1[N_time];
  int N2[N_time];
  int N3[N_time];
  int N4[N_time];
  int sumN1;
  int sumN2;
  int sumN3;
  int sumN4;
  vector[sumN1] Y1;
  vector[sumN2] Y2;
  vector[sumN3] Y3;
  vector[sumN4] Y4;
}

parameters {
  vector[N_time] mu1;
  real diff02;
  real diff03;
  real diff04;
  vector<lower=-pi()/2, upper=pi()/2>[N_time-1] diff_unif2;   // Uniform distribution
  vector<lower=-pi()/2, upper=pi()/2>[N_time-1] diff_unif3;   // Uniform distribution
  vector<lower=-pi()/2, upper=pi()/2>[N_time-1] diff_unif4;   // Uniform distribution
  real<lower=0> s_mu1;
  real<lower=0> s_diff2;
  real<lower=0> s_diff3;
  real<lower=0> s_diff4;
  real<lower=0> s_Y;
}

transformed parameters {
  vector[N_time] diff2;
  vector[N_time] diff3;
  vector[N_time] diff4;
  vector[N_time] mu2;
  vector[N_time] mu3;
  vector[N_time] mu4;
  diff2[1] = diff02;
  diff3[1] = diff03;
  diff4[1] = diff04;
  for (t in 2:N_time) {
    diff2[t] = diff2[t-1] + s_diff2*tan(diff_unif2[t-1]);    // diff follows cauchy distribution
  }
  for (t in 2:N_time) {
    diff3[t] = diff3[t-1] + s_diff3*tan(diff_unif3[t-1]);    // diff follows cauchy distribution
  }
  for (t in 2:N_time) {
    diff4[t] = diff4[t-1] + s_diff4*tan(diff_unif4[t-1]);    // diff follows cauchy distribution
  }
  mu2 = mu1 + diff2;
  mu3 = mu1 + diff3;
  mu4 = mu1 + diff4;
}

model {
  // State equation of mu1
  for (t in 3:N_time) {
    mu1[t] ~ normal(2*mu1[t-1] - mu1[t-2], s_mu1);
  }
  // Observation equation of Y1
  for (n in 1:N1[1]) {                   // N1=c(6,6,6,...)
    Y1[n] ~ normal(mu1[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N1[t]) {
      Y1[sum(N1[1:(t-1)])+n] ~ normal(mu1[t], s_Y);
    }
  }
  // Observation equation of Y2
  for (n in 1:N2[1]) {                   // N2=c(6,6,6,...)
    Y2[n] ~ normal(mu2[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N2[t]) {
      Y2[sum(N2[1:(t-1)])+n] ~ normal(mu2[t], s_Y);
    }
  }
  // Observation equation of Y3
  for (n in 1:N3[1]) {                   // N3=c(6,6,6,...)
    Y3[n] ~ normal(mu3[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N3[t]) {
      Y3[sum(N3[1:(t-1)])+n] ~ normal(mu3[t], s_Y);
    }
  }
  // Observation equation of Y4
  for (n in 1:N4[1]) {                   // N4=c(6,6,6,...)
    Y4[n] ~ normal(mu4[1], s_Y);
  }
  for (t in 2:N_time) {
    for (n in 1:N4[t]) {
      Y4[sum(N4[1:(t-1)])+n] ~ normal(mu4[t], s_Y);
    }
  }
}
