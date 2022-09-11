

# Load libraries
library(rstan)
library(bayesplot)
library(ggpubr)
library(tidyverse)
library(patchwork)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())





##### Lacal data #####

# Preparation of data
data <- read.csv("data/local_data.csv")
data1 <- subset(data, Condition=="Ambient Conditions")
data2 <- subset(data, Condition=="Warm")
data3 <- subset(data, Condition=="Chill")
data4 <- subset(data, Condition=="Low light")



### AhgCCA1
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  N3 = as.numeric(table(data3$Time)),
  N4 = as.numeric(table(data4$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  sumN3 = sum(as.numeric(table(data3$Time))),
  sumN4 = sum(as.numeric(table(data4$Time))),
  Y1 = data1$CCA1,
  Y2 = data2$CCA1,
  Y3 = data3$CCA1,
  Y4 = data4$CCA1
)

# SSM model
stanmodel_CCA1_local <- stan(
  file = "stan_model/SSM_diff_local.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$CCA1, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_CCA1_local)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_CCA1_local, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff2[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_CCA1_local)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["mu3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df4 <- data.frame(
  t(apply(mcmc_sample[["mu4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df5 <- data.frame(
  t(apply(mcmc_sample[["diff2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df6 <- data.frame(
  t(apply(mcmc_sample[["diff3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df7 <- data.frame(
  t(apply(mcmc_sample[["diff4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgCCA1_local <- cbind(result_df1, result_df2, result_df3, result_df4, 
                                 result_df5, result_df6, result_df7)
result_df_AhgCCA1_local$time <- unique(data$Time)
names(result_df_AhgCCA1_local) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                  "mu2_2.5", "mu2_50", "mu2_97.5",
                                  "mu3_2.5", "mu3_50", "mu3_97.5",
                                  "mu4_2.5", "mu4_50", "mu4_97.5",
                                  "diff2_2.5", "diff2_50", "diff2_97.5",
                                  "diff3_2.5", "diff3_50", "diff3_97.5",
                                  "diff4_2.5", "diff4_50", "diff4_97.5",
                                  "time")
result_df_AhgCCA1_local_rev <- cbind(
  result_df_AhgCCA1_local,
  data.frame(
    data_A = tapply(data1$CCA1, data1$Time, mean),
    data_W = tapply(data2$CCA1, data2$Time, mean),
    data_C = tapply(data3$CCA1, data3$Time, mean),
    data_L = tapply(data4$CCA1, data4$Time, mean)
  )
)



### AhgSIG5
data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  N3 = as.numeric(table(data3$Time)),
  N4 = as.numeric(table(data4$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  sumN3 = sum(as.numeric(table(data3$Time))),
  sumN4 = sum(as.numeric(table(data4$Time))),
  Y1 = data1$SIG5,
  Y2 = data2$SIG5,
  Y3 = data3$SIG5,
  Y4 = data4$SIG5
)

# SSM model
stanmodel_SIG5_local <- stan(
  file = "stan_model/SSM_diff_local.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$SIG5, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_SIG5_local)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_SIG5_local, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff2[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_SIG5_local)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["mu3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df4 <- data.frame(
  t(apply(mcmc_sample[["mu4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df5 <- data.frame(
  t(apply(mcmc_sample[["diff2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df6 <- data.frame(
  t(apply(mcmc_sample[["diff3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df7 <- data.frame(
  t(apply(mcmc_sample[["diff4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgSIG5_local <- cbind(result_df1, result_df2, result_df3, result_df4, 
                                 result_df5, result_df6, result_df7)
result_df_AhgSIG5_local$time <- unique(data$Time)
names(result_df_AhgSIG5_local) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                    "mu2_2.5", "mu2_50", "mu2_97.5",
                                    "mu3_2.5", "mu3_50", "mu3_97.5",
                                    "mu4_2.5", "mu4_50", "mu4_97.5",
                                    "diff2_2.5", "diff2_50", "diff2_97.5",
                                    "diff3_2.5", "diff3_50", "diff3_97.5",
                                    "diff4_2.5", "diff4_50", "diff4_97.5",
                                    "time")
result_df_AhgSIG5_local_rev <- cbind(
  result_df_AhgSIG5_local,
  data.frame(
    data_A = tapply(data1$SIG5, data1$Time, mean),
    data_W = tapply(data2$SIG5, data2$Time, mean),
    data_C = tapply(data3$SIG5, data3$Time, mean),
    data_L = tapply(data4$SIG5, data4$Time, mean)
  )
)



### AhgBLRP

# Preparation of data
data <- read.csv("data/local_data.csv")
nrow(data)
data <- na.omit(data)
nrow(data)
data1 <- subset(data, Condition=="Ambient Conditions")
data2 <- subset(data, Condition=="Warm")
data3 <- subset(data, Condition=="Chill")
data4 <- subset(data, Condition=="Low light")

data_list <- list(
  N_time = length(unique(data$Time)),
  N1 = as.numeric(table(data1$Time)),
  N2 = as.numeric(table(data2$Time)),
  N3 = as.numeric(table(data3$Time)),
  N4 = as.numeric(table(data4$Time)),
  sumN1 = sum(as.numeric(table(data1$Time))),
  sumN2 = sum(as.numeric(table(data2$Time))),
  sumN3 = sum(as.numeric(table(data3$Time))),
  sumN4 = sum(as.numeric(table(data4$Time))),
  Y1 = data1$BLRP,
  Y2 = data2$BLRP,
  Y3 = data3$BLRP,
  Y4 = data4$BLRP
)

# SSM model
stanmodel_BLRP_local <- stan(
  file = "stan_model/SSM_diff_local.stan",
  data = data_list,
  init = function() { list(mu1 = as.numeric(tapply(data1$BLRP, data1$Time, mean))) },
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)
print(stanmodel_BLRP_local)

# Confirmation of convergence of MCMC
mcmc_sample <- rstan::extract(stanmodel_BLRP_local, permuted = F)
mcmc_combo(mcmc_sample, pars = c("mu1[1]", "diff2[1]"))

# Visualization of the results
mcmc_sample <- rstan::extract(stanmodel_BLRP_local)
result_df1 <- data.frame(
  t(apply(mcmc_sample[["mu1"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df2 <- data.frame(
  t(apply(mcmc_sample[["mu2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df3 <- data.frame(
  t(apply(mcmc_sample[["mu3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df4 <- data.frame(
  t(apply(mcmc_sample[["mu4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df5 <- data.frame(
  t(apply(mcmc_sample[["diff2"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df6 <- data.frame(
  t(apply(mcmc_sample[["diff3"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df7 <- data.frame(
  t(apply(mcmc_sample[["diff4"]], 2, quantile, probs = c(0.025, 0.5, 0.975)))
)
result_df_AhgBLRP_local <- cbind(result_df1, result_df2, result_df3, result_df4, 
                                 result_df5, result_df6, result_df7)
result_df_AhgBLRP_local$time <- unique(data$Time)
names(result_df_AhgBLRP_local) <- c("mu1_2.5", "mu1_50", "mu1_97.5", 
                                    "mu2_2.5", "mu2_50", "mu2_97.5",
                                    "mu3_2.5", "mu3_50", "mu3_97.5",
                                    "mu4_2.5", "mu4_50", "mu4_97.5",
                                    "diff2_2.5", "diff2_50", "diff2_97.5",
                                    "diff3_2.5", "diff3_50", "diff3_97.5",
                                    "diff4_2.5", "diff4_50", "diff4_97.5",
                                    "time")
result_df_AhgBLRP_local_rev <- cbind(
  result_df_AhgBLRP_local,
  data.frame(
    data_A = tapply(data1$BLRP, data1$Time, mean),
    data_W = tapply(data2$BLRP, data2$Time, mean),
    data_C = tapply(data3$BLRP, data3$Time, mean),
    data_L = tapply(data4$BLRP, data4$Time, mean)
  )
)





##### Summarization of the results #####

# Create folder
if(file.exists("SSM_out/")==F){
  dir.create("SSM_out/", recursive=T)
}

# Save output
save.image("SSM_out/SSM_diff_local_out.RData")

# Load output
load("SSM_out/SSM_diff_local_out.RData")

# Create folder
if(file.exists("figure/SSM_diff/")==F){
  dir.create("figure/SSM_diff/", recursive=T)
}

# Load plot function
source("functions/Plot_functions.R")

# Visualization
glist_AhgCCA1_local <- diff_vis_local(df = result_df_AhgCCA1_local_rev, var = "AhgCCA1")
glist_AhgSIG5_local <- diff_vis_local(df = result_df_AhgSIG5_local_rev, var = "AhgSIG5")
glist_AhgBLRP_local <- diff_vis_local(df = result_df_AhgBLRP_local_rev, var = "AhgpsbD BLRP")

# Integration of all plots into a figure
glist <- c(glist_AhgCCA1_local, glist_AhgSIG5_local, glist_AhgBLRP_local)
g <- ((glist[[1]] / glist[[2]]) | (glist[[3]] / glist[[4]])) / 
  ((glist[[5]] / glist[[6]]) | (legend_local2 + plot_layout(tag_level = "new"))) +
  plot_layout(nrow = 3, byrow = F) +   # Adjust plot height by nrow
  plot_annotation(title = "Fig. Szz",
                  tag_levels = "A")

ggsave("figure/SSM_diff/SSM_diff_local.pdf",
       g, height = 297, width = 210, units = "mm")

