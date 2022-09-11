

# Load libraries
library(tidyverse)
library(data.table)
library(cmdstanr)
set_cmdstan_path("~/cmdstan/")



### Preparation of data

# Load data of March and September
data <- read.csv("data/march_data.csv")
data_MarSun <- subset(data, Condition=="Sun")
data_MarShade <- subset(data, Condition=="Shade")
data <- read.csv("data/september_data.csv")
data_SepSun <- subset(data, Condition=="Sun")
data_SepShade <- subset(data, Condition=="Shade")
environment_data_march <- read.csv("data/environment_data_corrected_march.csv")
environment_data_march$Condition = factor(environment_data_march$Condition, levels=c("Sun", "Shade"))
environment_data_march <- environment_data_march %>%
  arrange(Condition, Time)
environment_data_september <- read.csv("data/environment_data_corrected_september.csv")
environment_data_september$Condition = factor(environment_data_september$Condition, levels=c("Sun", "Shade"))
environment_data_september <- environment_data_september %>%
  arrange(Condition, Time)
env_MarSun <- environment_data_march[environment_data_march$Condition=="Sun",]
env_MarShade <- environment_data_march[environment_data_march$Condition=="Shade",]
env_SepSun <- environment_data_september[environment_data_september$Condition=="Sun",]
env_SepShade <- environment_data_september[environment_data_september$Condition=="Shade",]

env_MarSun2 <- NULL; env_MarShade2 <- NULL
env_SepSun2 <- NULL; env_SepShade2 <- NULL
for(i in 1:length(unique(data_MarSun$Time))){
  env_MarSun2 <- rbind(env_MarSun2, env_MarSun[env_MarSun$Time == ts(unique(data_MarSun$Time))[i],])
  env_MarShade2 <- rbind(env_MarShade2, env_MarShade[env_MarShade$Time == ts(unique(data_MarShade$Time))[i],])
  env_SepSun2 <- rbind(env_SepSun2, env_SepSun[env_SepSun$Time == ts(unique(data_SepSun$Time))[i],])
  env_SepShade2 <- rbind(env_SepShade2, env_SepShade[env_SepShade$Time == ts(unique(data_SepShade$Time))[i],])
}

env_MarSun2 <- rbind(env_MarSun2, env_MarSun[nrow(env_MarSun),])
env_MarShade2 <- rbind(env_MarShade2, env_MarShade[nrow(env_MarShade),])
env_SepSun2 <- rbind(env_SepSun2, env_SepSun[nrow(env_SepSun),])
env_SepShade2 <- rbind(env_SepShade2, env_SepShade[nrow(env_SepShade),])
env_SepSun2 <- rbind(env_SepSun[1,], env_SepSun2)

# Lag of environmental effect
Lag_env <- expand.grid(list(
  Lag_temp=c(0),
  Lag_light=c(0)))
  

## Load stan model
model <- cmdstan_model("stan_model/SSM_regression_MarSep_CCA1_muCommon_WAIC.stan")


## Function
quantile99 <- function(x){
  quantile(x, probs = c(0.005, 0.025, 0.5, 0.975, 0.995), names = TRUE)
}


### SSM for AhgCCA1
for(i in 1){
  data_list <- list(
    N_time = length(unique(data_MarSun$Time)),
    Lag_temp = Lag_env$Lag_temp[i],
    Lag_light = Lag_env$Lag_light[i],
    max_Lag = max(c(Lag_env$Lag_temp[i], Lag_env$Lag_light[i])),
    N_MarSun = as.numeric(table(data_MarSun$Time)),
    N_MarShade = as.numeric(table(data_MarShade$Time)),
    N_SepSun = as.numeric(table(data_SepSun$Time)),
    N_SepShade = as.numeric(table(data_SepShade$Time)),
    sumN_MarSun = sum(as.numeric(table(data_MarSun$Time))),
    sumN_MarShade = sum(as.numeric(table(data_MarShade$Time))),
    sumN_SepSun = sum(as.numeric(table(data_SepSun$Time))),
    sumN_SepShade = sum(as.numeric(table(data_SepShade$Time))),
    temp_MarSun = env_MarSun2$Temperature,
    temp_MarShade = env_MarShade2$Temperature,
    temp_SepSun = env_SepSun2$Temperature,
    temp_SepShade = env_SepShade2$Temperature,
    light_MarSun = env_MarSun2$Irradiance,
    light_MarShade = env_MarShade2$Irradiance,
    light_SepSun = env_SepSun2$Irradiance,
    light_SepShade = env_SepShade2$Irradiance,
    Y_MarSun = data_MarSun$CCA1,
    Y_MarShade = data_MarShade$CCA1,
    Y_SepSun = data_SepSun$CCA1,
    Y_SepShade = data_SepShade$CCA1
    )
  
  # SSM model
  fit <- model$sample(
    data = data_list,
    init = function() { list(alpha_MarSun = as.numeric(tapply(data_MarSun$CCA1, data_MarSun$Time, mean)),
                             alpha_MarShade = as.numeric(tapply(data_MarShade$CCA1, data_MarShade$Time, mean)),
                             alpha_SepSun = as.numeric(tapply(data_SepSun$CCA1, data_SepSun$Time, mean)),
                             alpha_SepShade = as.numeric(tapply(data_SepShade$CCA1, data_SepShade$Time, mean))) },
    seed = 10,
    iter_warmup = 3000,
    iter_sampling = 1000,
    #thin = 3,
    chains = 4,
    parallel_chains = 4,
    max_treedepth = 15,
    adapt_delta = 0.99,
    refresh = 1000,
    show_messages = F,
    sig_figs = 4,
    output_dir = "/tmp",
    output_basename = paste0("CCA1_WAIC_", i)
  )
  
  # 99% Bayesian credible intervals
  outcsv_name <- list.files("/tmp")
  outcsv_name <- outcsv_name[grep(paste0("CCA1_WAIC_", i), outcsv_name)]
  tmp_csv_b_temp <- NULL
  tmp_csv_b_light <- NULL
  tmp_csv_alpha_MarSun <- NULL
  tmp_csv_alpha_MarShade <- NULL
  tmp_csv_alpha_SepSun <- NULL
  tmp_csv_alpha_SepShade <- NULL
  tmp_csv_log_lik_MarSun <- NULL
  tmp_csv_log_lik_MarShade <- NULL
  tmp_csv_log_lik_SepSun <- NULL
  tmp_csv_log_lik_SepShade <- NULL
  for(j in 1:length(outcsv_name)){
    tmp_csv <- as.data.frame(fread(cmd = paste0("grep -v '^#' ", "/tmp/", outcsv_name[j])))
    tmp_csv_b_temp <- rbind(tmp_csv_b_temp, tmp_csv[,str_starts(names(tmp_csv), "b_temp")])
    tmp_csv_b_light <- rbind(tmp_csv_b_light, tmp_csv[,str_starts(names(tmp_csv), "b_light")])
    tmp_csv_alpha_MarSun <- rbind(tmp_csv_alpha_MarSun, tmp_csv[,str_starts(names(tmp_csv), "alpha_MarSun")])
    tmp_csv_alpha_MarShade <- rbind(tmp_csv_alpha_MarShade, tmp_csv[,str_starts(names(tmp_csv), "alpha_MarShade")])
    tmp_csv_alpha_SepSun <- rbind(tmp_csv_alpha_SepSun, tmp_csv[,str_starts(names(tmp_csv), "alpha_SepSun")])
    tmp_csv_alpha_SepShade <- rbind(tmp_csv_alpha_SepShade, tmp_csv[,str_starts(names(tmp_csv), "alpha_SepShade")])
    tmp_csv_log_lik_MarSun <- rbind(tmp_csv_log_lik_MarSun, tmp_csv[,str_starts(names(tmp_csv), "log_lik_MarSun")])
    tmp_csv_log_lik_MarShade <- rbind(tmp_csv_log_lik_MarShade, tmp_csv[,str_starts(names(tmp_csv), "log_lik_MarShade")])
    tmp_csv_log_lik_SepSun <- rbind(tmp_csv_log_lik_SepSun, tmp_csv[,str_starts(names(tmp_csv), "log_lik_SepSun")])
    tmp_csv_log_lik_SepShade <- rbind(tmp_csv_log_lik_SepShade, tmp_csv[,str_starts(names(tmp_csv), "log_lik_SepShade")])
  }
  
  df_b_temp <- as.data.frame(round(t(apply(tmp_csv_b_temp, 2, quantile99)), digits = 4))
  df_b_light <- as.data.frame(round(t(apply(tmp_csv_b_light, 2, quantile99)), digits = 4))
  df_alpha_MarSun <- as.data.frame(round(t(apply(tmp_csv_alpha_MarSun, 2, quantile99)), digits = 4))
  df_alpha_MarShade <- as.data.frame(round(t(apply(tmp_csv_alpha_MarShade, 2, quantile99)), digits = 4))
  df_alpha_SepSun <- as.data.frame(round(t(apply(tmp_csv_alpha_SepSun, 2, quantile99)), digits = 4))
  df_alpha_SepShade <- as.data.frame(round(t(apply(tmp_csv_alpha_SepShade, 2, quantile99)), digits = 4))
  
  df_all <- rbind(df_b_temp, df_b_light, df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
  df_all <- df_all %>%
    mutate(par = row.names(df_all)) %>%
    relocate(par)
  
  fwrite(df_all,
         file = paste0("SSM_out/SSM_regression_MarSep_CCA1_muCommon_out_", 
                       "Lagtemp", data_list$Lag_temp, 
                       "_Laglight", data_list$Lag_light, 
                       "_WAIC_99.csv"))
  
  # MCMC samples of alpha
  tmp_csv_alpha <- cbind(tmp_csv_alpha_MarSun, tmp_csv_alpha_MarShade, tmp_csv_alpha_SepSun, tmp_csv_alpha_SepShade)
  fwrite(tmp_csv_alpha,
         file = paste0("SSM_out/SSM_regression_MarSep_CCA1_muCommon_out_", 
                       "Lagtemp", data_list$Lag_temp, 
                       "_Laglight", data_list$Lag_light, 
                       "_WAIC_MCMCalpha.csv"))
  
  # log likelihood
  df_log_lik <- as.data.frame(cbind(tmp_csv_log_lik_MarSun, tmp_csv_log_lik_MarShade, 
                                    tmp_csv_log_lik_SepSun, tmp_csv_log_lik_SepShade))
  
  fwrite(df_log_lik,
         file = paste0("SSM_out/SSM_regression_MarSep_CCA1_muCommon_out_", 
                       "Lagtemp", data_list$Lag_temp, 
                       "_Laglight", data_list$Lag_light, 
                       "_WAIC_loglik.csv"))

  file.remove(paste0("/tmp/", outcsv_name))
  rm(data_list, fit, df_all, df_log_lik)
  gc(reset = T); gc(reset = T)
}

