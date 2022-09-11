

## Load packages
library(tidyverse)
library(statnet.common)
library(data.table)


## Load data of March and September
data <- read.csv("data/march_data.csv")
data_MarSun <- subset(data, Condition=="Sun")
data_MarShade <- subset(data, Condition=="Shade")
data <- read.csv("data/september_data.csv")
data_SepSun <- subset(data, Condition=="Sun")
data_SepShade <- subset(data, Condition=="Shade")

SIG5_MarSun = data_MarSun %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_MarShade = data_MarShade %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_SepSun = data_SepSun %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_SepShade = data_SepShade %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))

BLRP_MarSun = data_MarSun %>%
  group_by(Time) %>%
  summarize(Mean_BLRP = mean(BLRP))
BLRP_MarShade = data_MarShade %>%
  group_by(Time) %>%
  summarize(Mean_BLRP = mean(BLRP))
BLRP_SepSun = data_SepSun %>%
  group_by(Time) %>%
  summarize(Mean_BLRP = mean(BLRP))
BLRP_SepShade = data_SepShade %>%
  group_by(Time) %>%
  summarize(Mean_BLRP = mean(BLRP))


## Functions
quantile99 <- function(x){
  quantile(x, probs = c(0.005, 0.025, 0.5, 0.975, 0.995), names = TRUE)
}

rmse <- function(x, y){
  sqrt(mean((x - y)^2))
}


## Lag of environmental effect
Lag_env_SIG5 <- expand.grid(list(
  Lag_temp=c(0),
  Lag_light=c(0),
  Lag_CCA1=c(0, 1, 2, 3, 4)))

Lag_env_BLRP <- expand.grid(list(
  Lag_temp=c(0),
  Lag_light=c(0),
  Lag_SIG5=c(0, 1, 2, 3, 4)))





##### March #####

## Prepare container of results
df_cor_rmse_loglik_SIG5_Mar <- cbind(Lag_env_SIG5, 
                                     data.frame(cor_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_mean = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_mean = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_mean = rep(NA, length = nrow(Lag_env_SIG5))))
df_cor_rmse_loglik_BLRP_Mar <- cbind(Lag_env_BLRP, 
                                     data.frame(cor_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_mean = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_mean = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_mean = rep(NA, length = nrow(Lag_env_BLRP))))


## SIG5
for(i in 1:nrow(Lag_env_SIG5)){
  # Load mcmc samples
  df_mcmc <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Mar_SIG5_muCommon_out_", 
                                        "Lagtemp", Lag_env_SIG5$Lag_temp[i],
                                        "_Laglight", Lag_env_SIG5$Lag_light[i],
                                        "_LagCCA1", Lag_env_SIG5$Lag_CCA1[i],
                                        "_WAIC_rev2_MCMCalpha.csv")))
  
  # Data after the max lag
  max_lag <- max(Lag_env_SIG5[i,])
  use_idx <- which(!(
    str_ends(names(df_mcmc), paste0("\\.", max_lag)) | 
      str_ends(names(df_mcmc), paste0("\\.", max_lag-1)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-2)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-3)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-4))
  ))
  
  # Correlation
  cor_mcmc_obs <- NULL
  obs <- c(SIG5_MarSun$Mean_SIG5, SIG5_MarShade$Mean_SIG5)
  for(j in 1:nrow(df_mcmc)){
    cor_mcmc_obs[j] <- cor(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_SIG5_Mar[i,4:6] <- quantile99(cor_mcmc_obs)[2:4]
  df_cor_rmse_loglik_SIG5_Mar[i,7] <- mean(cor_mcmc_obs)
  
  # RMSE
  rmse_mcmc_obs <- NULL
  obs <- c(SIG5_MarSun$Mean_SIG5, SIG5_MarShade$Mean_SIG5)
  for(j in 1:nrow(df_mcmc)){
    rmse_mcmc_obs[j] <- rmse(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_SIG5_Mar[i,8:10] <- quantile99(rmse_mcmc_obs)[2:4]
  df_cor_rmse_loglik_SIG5_Mar[i,11] <- mean(rmse_mcmc_obs)
  
  # Log likelihood
  df_log_lik <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Mar_SIG5_muCommon_out_", 
                                           "Lagtemp", Lag_env_SIG5$Lag_temp[i],
                                           "_Laglight", Lag_env_SIG5$Lag_light[i],
                                           "_LagCCA1", Lag_env_SIG5$Lag_CCA1[i],
                                           "_WAIC_rev2_loglik.csv")))
  df_log_lik <- df_log_lik %>% select_if(negate(anyNA))
  df_cor_rmse_loglik_SIG5_Mar[i,12:14] <- quantile99(rowMeans(df_log_lik))[2:4]
  df_cor_rmse_loglik_SIG5_Mar[i,15] <- mean(rowMeans(df_log_lik))
}


## BLRP
for(i in 1:nrow(Lag_env_BLRP)){
  # Load mcmc samples
  df_mcmc <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Mar_BLRP_muCommon_out_", 
                                        "Lagtemp", Lag_env_BLRP$Lag_temp[i],
                                        "_Laglight", Lag_env_BLRP$Lag_light[i],
                                        "_LagSIG5", Lag_env_BLRP$Lag_SIG5[i],
                                        "_WAIC_rev2_MCMCalpha.csv")))
  
  # Data after the max lag
  max_lag <- max(Lag_env_BLRP[i,])
  use_idx <- which(!(
    str_ends(names(df_mcmc), paste0("\\.", max_lag)) | 
      str_ends(names(df_mcmc), paste0("\\.", max_lag-1)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-2)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-3)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-4))
  ))
  
  # Correlation
  cor_mcmc_obs <- NULL
  obs <- c(BLRP_MarSun$Mean_BLRP, BLRP_MarShade$Mean_BLRP)
  for(j in 1:nrow(df_mcmc)){
    cor_mcmc_obs[j] <- cor(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_BLRP_Mar[i,4:6] <- quantile99(cor_mcmc_obs)[2:4]
  df_cor_rmse_loglik_BLRP_Mar[i,7] <- mean(cor_mcmc_obs)
  
  # RMSE
  rmse_mcmc_obs <- NULL
  obs <- c(BLRP_MarSun$Mean_BLRP, BLRP_MarShade$Mean_BLRP)
  for(j in 1:nrow(df_mcmc)){
    rmse_mcmc_obs[j] <- rmse(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_BLRP_Mar[i,8:10] <- quantile99(rmse_mcmc_obs)[2:4]
  df_cor_rmse_loglik_BLRP_Mar[i,11] <- mean(rmse_mcmc_obs)
  
  # Log likelihood
  df_log_lik <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Mar_BLRP_muCommon_out_", 
                                           "Lagtemp", Lag_env_BLRP$Lag_temp[i],
                                           "_Laglight", Lag_env_BLRP$Lag_light[i],
                                           "_LagSIG5", Lag_env_BLRP$Lag_SIG5[i],
                                           "_WAIC_rev2_loglik.csv")))
  df_log_lik <- df_log_lik %>% select_if(negate(anyNA))
  df_cor_rmse_loglik_BLRP_Mar[i,12:14] <- quantile99(rowMeans(df_log_lik))[2:4]
  df_cor_rmse_loglik_BLRP_Mar[i,15] <- mean(rowMeans(df_log_lik))
}





##### September #####

## Prepare container of results
df_cor_rmse_loglik_SIG5_Sep <- cbind(Lag_env_SIG5, 
                                     data.frame(cor_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                cor_mean = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                rmse_mean = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_2.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_50 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_97.5 = rep(NA, length = nrow(Lag_env_SIG5)),
                                                loglik_mean = rep(NA, length = nrow(Lag_env_SIG5))))
df_cor_rmse_loglik_BLRP_Sep <- cbind(Lag_env_BLRP, 
                                     data.frame(cor_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                cor_mean = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                rmse_mean = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_2.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_50 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_97.5 = rep(NA, length = nrow(Lag_env_BLRP)),
                                                loglik_mean = rep(NA, length = nrow(Lag_env_BLRP))))


## SIG5
for(i in 1:nrow(Lag_env_SIG5)){
  # Load mcmc samples
  df_mcmc <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Sep_SIG5_muCommon_out_", 
                                        "Lagtemp", Lag_env_SIG5$Lag_temp[i],
                                        "_Laglight", Lag_env_SIG5$Lag_light[i],
                                        "_LagCCA1", Lag_env_SIG5$Lag_CCA1[i],
                                        "_WAIC_rev2_MCMCalpha.csv")))
  
  # Data after the max lag
  max_lag <- max(Lag_env_SIG5[i,])
  use_idx <- which(!(
    str_ends(names(df_mcmc), paste0("\\.", max_lag)) | 
      str_ends(names(df_mcmc), paste0("\\.", max_lag-1)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-2)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-3)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-4))
  ))
  
  # Correlation
  cor_mcmc_obs <- NULL
  obs <- c(SIG5_SepSun$Mean_SIG5, SIG5_SepShade$Mean_SIG5)
  for(j in 1:nrow(df_mcmc)){
    cor_mcmc_obs[j] <- cor(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_SIG5_Sep[i,4:6] <- quantile99(cor_mcmc_obs)[2:4]
  df_cor_rmse_loglik_SIG5_Sep[i,7] <- mean(cor_mcmc_obs)
  
  # RMSE
  rmse_mcmc_obs <- NULL
  obs <- c(SIG5_SepSun$Mean_SIG5, SIG5_SepShade$Mean_SIG5)
  for(j in 1:nrow(df_mcmc)){
    rmse_mcmc_obs[j] <- rmse(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_SIG5_Sep[i,8:10] <- quantile99(rmse_mcmc_obs)[2:4]
  df_cor_rmse_loglik_SIG5_Sep[i,11] <- mean(rmse_mcmc_obs)
  
  # Log likelihood
  df_log_lik <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Sep_SIG5_muCommon_out_", 
                                           "Lagtemp", Lag_env_SIG5$Lag_temp[i],
                                           "_Laglight", Lag_env_SIG5$Lag_light[i],
                                           "_LagCCA1", Lag_env_SIG5$Lag_CCA1[i],
                                           "_WAIC_rev2_loglik.csv")))
  df_log_lik <- df_log_lik %>% select_if(negate(anyNA))
  df_cor_rmse_loglik_SIG5_Sep[i,12:14] <- quantile99(rowMeans(df_log_lik))[2:4]
  df_cor_rmse_loglik_SIG5_Sep[i,15] <- mean(rowMeans(df_log_lik))
}


## BLRP
for(i in 1:nrow(Lag_env_BLRP)){
  # Load mcmc samples
  df_mcmc <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Sep_BLRP_muCommon_out_", 
                                        "Lagtemp", Lag_env_BLRP$Lag_temp[i],
                                        "_Laglight", Lag_env_BLRP$Lag_light[i],
                                        "_LagSIG5", Lag_env_BLRP$Lag_SIG5[i],
                                        "_WAIC_rev2_MCMCalpha.csv")))
  
  # Data after the max lag
  max_lag <- max(Lag_env_BLRP[i,])
  use_idx <- which(!(
    str_ends(names(df_mcmc), paste0("\\.", max_lag)) | 
      str_ends(names(df_mcmc), paste0("\\.", max_lag-1)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-2)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-3)) |
      str_ends(names(df_mcmc), paste0("\\.", max_lag-4))
  ))
  
  # Correlation
  cor_mcmc_obs <- NULL
  obs <- c(BLRP_SepSun$Mean_BLRP, BLRP_SepShade$Mean_BLRP)
  for(j in 1:nrow(df_mcmc)){
    cor_mcmc_obs[j] <- cor(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_BLRP_Sep[i,4:6] <- quantile99(cor_mcmc_obs)[2:4]
  df_cor_rmse_loglik_BLRP_Sep[i,7] <- mean(cor_mcmc_obs)
  
  # RMSE
  rmse_mcmc_obs <- NULL
  obs <- c(BLRP_SepSun$Mean_BLRP, BLRP_SepShade$Mean_BLRP)
  for(j in 1:nrow(df_mcmc)){
    rmse_mcmc_obs[j] <- rmse(as.numeric(df_mcmc[j,use_idx]), obs[use_idx])
  }
  df_cor_rmse_loglik_BLRP_Sep[i,8:10] <- quantile99(rmse_mcmc_obs)[2:4]
  df_cor_rmse_loglik_BLRP_Sep[i,11] <- mean(rmse_mcmc_obs)
  
  # Log likelihood
  df_log_lik <- as.data.frame(fread(paste0("SSM_out/SSM_regression_Sep_BLRP_muCommon_out_", 
                                           "Lagtemp", Lag_env_BLRP$Lag_temp[i],
                                           "_Laglight", Lag_env_BLRP$Lag_light[i],
                                           "_LagSIG5", Lag_env_BLRP$Lag_SIG5[i],
                                           "_WAIC_rev2_loglik.csv")))
  df_log_lik <- df_log_lik %>% select_if(negate(anyNA))
  df_cor_rmse_loglik_BLRP_Sep[i,12:14] <- quantile99(rowMeans(df_log_lik))[2:4]
  df_cor_rmse_loglik_BLRP_Sep[i,15] <- mean(rowMeans(df_log_lik))
}





##### Save output #####
save.image("data/cor_RMSE_loglik_Mar_or_Sep_LagTempLight0_rev2.RData")

