

# Load libraries
library(rstan)
library(bayesplot)
library(ggpubr)
library(tidyverse)
library(patchwork)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())



##### March data #####

# Preparation of data
data <- read.csv("data/march_data.csv")
data1 <- subset(data, Condition=="Sun")
data2 <- subset(data, Condition=="Shade")



### AhgCCA1
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$CCA1,
  Y2 = data2$CCA1
)

# SSM model
stanmodel_CCA1_Mar <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$CCA1, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_CCA1_Mar)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_CCA1_Mar, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_CCA1_Mar)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgCCA1_Mar <- cbind(result_df1, result_df2, result_df3)
result_df_AhgCCA1_Mar$time <- unique(data$Time)
names(result_df_AhgCCA1_Mar) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgCCA1_Mar_rev <- cbind(
  result_df_AhgCCA1_Mar,
  data.frame(
    data_Sun = tapply(data1$CCA1, data1$Time, mean),
    data_Shade = tapply(data2$CCA1, data2$Time, mean)
  )
)



### AhgSIG5
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$SIG5,
  Y2 = data2$SIG5
)

# SSM model
stanmodel_SIG5_Mar <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$SIG5, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_SIG5_Mar)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_SIG5_Mar, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_SIG5_Mar)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgSIG5_Mar <- cbind(result_df1, result_df2, result_df3)
result_df_AhgSIG5_Mar$time <- unique(data$Time)
names(result_df_AhgSIG5_Mar) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgSIG5_Mar_rev <- cbind(
  result_df_AhgSIG5_Mar,
  data.frame(
    data_Sun = tapply(data1$SIG5, data1$Time, mean),
    data_Shade = tapply(data2$SIG5, data2$Time, mean)
  )
)



### AhgpsbD BLRP
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$BLRP,
  Y2 = data2$BLRP
)

# SSM model
stanmodel_BLRP_Mar <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$BLRP, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_BLRP_Mar)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_BLRP_Mar, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_BLRP_Mar)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgBLRP_Mar <- cbind(result_df1, result_df2, result_df3)
result_df_AhgBLRP_Mar$time <- unique(data$Time)
names(result_df_AhgBLRP_Mar) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgBLRP_Mar_rev <- cbind(
  result_df_AhgBLRP_Mar,
  data.frame(
    data_Sun = tapply(data1$BLRP, data1$Time, mean),
    data_Shade = tapply(data2$BLRP, data2$Time, mean)
  )
)





##### September data #####

# Preparation of data
data <- read.csv("data/september_data.csv")
data1 <- subset(data, Condition=="Sun")
data2 <- subset(data, Condition=="Shade")



### AhgCCA1
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$CCA1,
  Y2 = data2$CCA1
)

# SSM model
stanmodel_CCA1_Sep <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$CCA1, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_CCA1_Sep)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_CCA1_Sep, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_CCA1_Sep)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgCCA1_Sep <- cbind(result_df1, result_df2, result_df3)
result_df_AhgCCA1_Sep$time <- unique(data$Time)
names(result_df_AhgCCA1_Sep) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                              "mu2_2.5", "mu2_50", "mu2_97.5",
                              "diff_2.5", "diff_50", "diff_97.5",
                              "time")
result_df_AhgCCA1_Sep_rev <- cbind(
  result_df_AhgCCA1_Sep,
  data.frame(
    data_Sun = tapply(data1$CCA1, data1$Time, mean),
    data_Shade = tapply(data2$CCA1, data2$Time, mean)
  )
)



### AhgSIG5
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$SIG5,
  Y2 = data2$SIG5
)

# SSM model
stanmodel_SIG5_Sep <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$SIG5, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_SIG5_Sep)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_SIG5_Sep, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_SIG5_Sep)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgSIG5_Sep <- cbind(result_df1, result_df2, result_df3)
result_df_AhgSIG5_Sep$time <- unique(data$Time)
names(result_df_AhgSIG5_Sep) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                              "mu2_2.5", "mu2_50", "mu2_97.5",
                              "diff_2.5", "diff_50", "diff_97.5",
                              "time")
result_df_AhgSIG5_Sep_rev <- cbind(
  result_df_AhgSIG5_Sep,
  data.frame(
    data_Sun = tapply(data1$SIG5, data1$Time, mean),
    data_Shade = tapply(data2$SIG5, data2$Time, mean)
  )
)



### AhgpsbD BLRP
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  Y1 = data1$BLRP,
  Y2 = data2$BLRP
)

# SSM model
stanmodel_BLRP_Sep <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$BLRP, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_BLRP_Sep)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_BLRP_Sep, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_BLRP_Sep)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgBLRP_Sep <- cbind(result_df1, result_df2, result_df3)
result_df_AhgBLRP_Sep$time <- unique(data$Time)
names(result_df_AhgBLRP_Sep) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                              "mu2_2.5", "mu2_50", "mu2_97.5",
                              "diff_2.5", "diff_50", "diff_97.5",
                              "time")
result_df_AhgBLRP_Sep_rev <- cbind(
  result_df_AhgBLRP_Sep,
  data.frame(
    data_Sun = tapply(data1$BLRP, data1$Time, mean),
    data_Shade = tapply(data2$BLRP, data2$Time, mean)
  )
)





##### Summarization of the results #####

# Create folder
if(file.exists("SSM_out/")==F){
  dir.create("SSM_out/", recursive=T)
}

# Save output
save.image("SSM_out/SSM_diff_SunShade_out.RData")

# Load output
load("SSM_out/SSM_diff_SunShade_out.RData")

# Create folder
if(file.exists("figure/SSM_diff/")==F){
  dir.create("figure/SSM_diff/", recursive=T)
}

# Load plot function
source("functions/Plot_functions.R")

# Visualization
glist_AhgCCA1_Mar <- diff_vis_SunShade(df = result_df_AhgCCA1_Mar_rev, var = "AhgCCA1", season = "Mar")
glist_AhgSIG5_Mar <- diff_vis_SunShade(df = result_df_AhgSIG5_Mar_rev, var = "AhgSIG5", season = "Mar")
glist_AhgBLRP_Mar <- diff_vis_SunShade(df = result_df_AhgBLRP_Mar_rev, var = "AhgpsbD BLRP", season = "Mar")
glist_AhgCCA1_Sep <- diff_vis_SunShade(df = result_df_AhgCCA1_Sep_rev, var = "AhgCCA1", season = "Sep")
glist_AhgSIG5_Sep <- diff_vis_SunShade(df = result_df_AhgSIG5_Sep_rev, var = "AhgSIG5", season = "Sep")
glist_AhgBLRP_Sep <- diff_vis_SunShade(df = result_df_AhgBLRP_Sep_rev, var = "AhgpsbD BLRP", season = "Sep")

# Integration of all plots into a figure
glist <- c(glist_AhgCCA1_Mar, glist_AhgSIG5_Mar, glist_AhgBLRP_Mar, 
           glist_AhgCCA1_Sep, glist_AhgSIG5_Sep, glist_AhgBLRP_Sep)
g <- ((glist[[1]] / glist[[2]] / glist[[3]] / glist[[4]] / glist[[5]] / glist[[6]]) |
        (glist[[7]] / glist[[8]] / glist[[9]] / glist[[10]] / glist[[11]] / glist[[12]])) /
  (legend_SunShade1 + plot_layout(tag_level = "new")) +
  plot_layout(heights = c(20, 1)) +
  plot_annotation(title = "Fig. Sxx",
                  tag_levels = "A")

ggsave("figure/SSM_diff/SSM_diff_SunShade.pdf",
       g, height = 297, width = 210, units = "mm")

