

# Load libraries
library(rstan)
library(bayesplot)
library(ggpubr)
library(tidyverse)
library(patchwork)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())





##### Sun data #####

# Preparation of data
data <- read.csv("data/march_data.csv")
data1 <- subset(data, Condition=="Sun")
data <- read.csv("data/september_data.csv")
data2 <- subset(data, Condition=="Sun")


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
stanmodel_CCA1_Sun <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$CCA1, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_CCA1_Sun)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_CCA1_Sun, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_CCA1_Sun)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgCCA1_Sun <- cbind(result_df1, result_df2, result_df3)
result_df_AhgCCA1_Sun$time <- unique(data$Time)
names(result_df_AhgCCA1_Sun) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgCCA1_Sun_rev <- cbind(
  result_df_AhgCCA1_Sun,
  data.frame(
    data_Mar = tapply(data1$CCA1, data1$Time, mean),
    data_Sep = tapply(data2$CCA1, data2$Time, mean)
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
stanmodel_SIG5_Sun <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$SIG5, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_SIG5_Sun)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_SIG5_Sun, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_SIG5_Sun)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgSIG5_Sun <- cbind(result_df1, result_df2, result_df3)
result_df_AhgSIG5_Sun$time <- unique(data$Time)
names(result_df_AhgSIG5_Sun) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgSIG5_Sun_rev <- cbind(
  result_df_AhgSIG5_Sun,
  data.frame(
    data_Mar = tapply(data1$SIG5, data1$Time, mean),
    data_Sep = tapply(data2$SIG5, data2$Time, mean)
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
stanmodel_BLRP_Sun <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$BLRP, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_BLRP_Sun)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_BLRP_Sun, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_BLRP_Sun)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgBLRP_Sun <- cbind(result_df1, result_df2, result_df3)
result_df_AhgBLRP_Sun$time <- unique(data$Time)
names(result_df_AhgBLRP_Sun) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgBLRP_Sun_rev <- cbind(
  result_df_AhgBLRP_Sun,
  data.frame(
    data_Mar = tapply(data1$BLRP, data1$Time, mean),
    data_Sep = tapply(data2$BLRP, data2$Time, mean)
  )
)





##### Shade data #####

# Preparation of data
data <- read.csv("data/march_data.csv")
data1 <- subset(data, Condition=="Shade")
data <- read.csv("data/september_data.csv")
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
stanmodel_CCA1_Shade <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$CCA1, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_CCA1_Shade)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_CCA1_Shade, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_CCA1_Shade)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgCCA1_Shade <- cbind(result_df1, result_df2, result_df3)
result_df_AhgCCA1_Shade$time <- unique(data$Time)
names(result_df_AhgCCA1_Shade) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgCCA1_Shade_rev <- cbind(
  result_df_AhgCCA1_Shade,
  data.frame(
    data_Mar = tapply(data1$CCA1, data1$Time, mean),
    data_Sep = tapply(data2$CCA1, data2$Time, mean)
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
stanmodel_SIG5_Shade <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$SIG5, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_SIG5_Shade)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_SIG5_Shade, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_SIG5_Shade)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgSIG5_Shade <- cbind(result_df1, result_df2, result_df3)
result_df_AhgSIG5_Shade$time <- unique(data$Time)
names(result_df_AhgSIG5_Shade) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgSIG5_Shade_rev <- cbind(
  result_df_AhgSIG5_Shade,
  data.frame(
    data_Mar = tapply(data1$SIG5, data1$Time, mean),
    data_Sep = tapply(data2$SIG5, data2$Time, mean)
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
stanmodel_BLRP_Shade <- stan(
  file = "stan_model/SSM_diff_SunShade.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$BLRP, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_BLRP_Shade)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_BLRP_Shade, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_BLRP_Shade)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["diff"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgBLRP_Shade <- cbind(result_df1, result_df2, result_df3)
result_df_AhgBLRP_Shade$time <- unique(data$Time)
names(result_df_AhgBLRP_Shade) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "diff_2.5", "diff_50", "diff_97.5",
                                  "time")
result_df_AhgBLRP_Shade_rev <- cbind(
  result_df_AhgBLRP_Shade,
  data.frame(
    data_Mar = tapply(data1$BLRP, data1$Time, mean),
    data_Sep = tapply(data2$BLRP, data2$Time, mean)
  )
)




##### Summarization of the results #####

# Create folder
if(file.exists("SSM_out/")==F){
  dir.create("SSM_out/", recursive=T)
}

# Save output
save.image("SSM_out/SSM_diff_MarSep_out.RData")

# Load output
load("SSM_out/SSM_diff_MarSep_out.RData")

# Create folder
if(file.exists("figure/SSM_diff/")==F){
  dir.create("figure/SSM_diff/", recursive=T)
}

# Load plot function
source("functions/Plot_functions.R")

# Visualization
glist_AhgCCA1_Sun <- diff_vis_MarSep(df = result_df_AhgCCA1_Sun_rev, var = "AhgCCA1", condition = "Sun")
glist_AhgSIG5_Sun <- diff_vis_MarSep(df = result_df_AhgSIG5_Sun_rev, var = "AhgSIG5", condition = "Sun")
glist_AhgBLRP_Sun <- diff_vis_MarSep(df = result_df_AhgBLRP_Sun_rev, var = "AhgpsbD BLRP", condition = "Sun")
glist_AhgCCA1_Shade <- diff_vis_MarSep(df = result_df_AhgCCA1_Shade_rev, var = "AhgCCA1", condition = "Shade")
glist_AhgSIG5_Shade <- diff_vis_MarSep(df = result_df_AhgSIG5_Shade_rev, var = "AhgSIG5", condition = "Shade")
glist_AhgBLRP_Shade <- diff_vis_MarSep(df = result_df_AhgBLRP_Shade_rev, var = "AhgpsbD BLRP", condition = "Shade")

# Integration of all plots into a figure
glist <- c(glist_AhgCCA1_Sun, glist_AhgSIG5_Sun, glist_AhgBLRP_Sun, 
           glist_AhgCCA1_Shade, glist_AhgSIG5_Shade, glist_AhgBLRP_Shade)
g <- ((glist[[1]] / glist[[2]] / glist[[3]] / glist[[4]] / glist[[5]] / glist[[6]]) |
  (glist[[7]] / glist[[8]] / glist[[9]] / glist[[10]] / glist[[11]] / glist[[12]])) /
  (legend_MarSep1 + plot_layout(tag_level = "new")) +
  plot_layout(heights = c(20, 1)) +
  plot_annotation(title = "Fig. Syy",
                  tag_levels = "A")

ggsave("figure/SSM_diff/SSM_diff_MarSep.pdf",
       g, height = 297, width = 210, units = "mm")

