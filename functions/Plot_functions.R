
##### Visualization of Sun-Shade comparison #####

diff_vis_SunShade <- function(df, var, season){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 25; seq1 <- seq(0,25,5)
    ymin2 <- -4; ymax2 <- 8; seq2 <- seq(-4,8,4)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.2; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin2 <- -1; ymax2 <- 2.1; seq2 <- seq(-1,2,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 5.1; seq1 <- seq(0,5,2.5)
    ymin2 <- -1.5; ymax2 <- 3.5; seq2 <- seq(-1,3,1)
  }
  
  if(season=="Mar"){
    subtitle <- "March/Spring"
  }else if(season=="Sep"){
    subtitle <- "September/Autumn"
  }
  
  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = mu1_2.5, ymax = mu1_97.5), alpha = 0.3, fill = "orange") +
    geom_line(aes(y = mu1_50), col = "orange") +
    geom_point(aes(y = data_Sun), col = "orange", alpha = 0.6) +
    geom_ribbon(aes(ymin = mu2_2.5, ymax = mu2_97.5), alpha = 0.3, fill = "gray30") +
    geom_line(aes(y = mu2_50), col = "gray30") +
    geom_point(aes(y = data_Shade), col = "gray30", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = subtitle, 
         x = "",
         #x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = diff_2.5, ymax = diff_97.5), alpha = 0.5) +
    geom_line(aes(y = diff_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(
         #title = var, subtitle = subtitle, 
         x = "Time relative to initial dawn (h)", 
         y = "Difference in transcript\nabundance")
  
  return(list(g1, g2))
}





##### Visualization of Mar-Sep comparison #####

diff_vis_MarSep <- function(df, var, condition){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 25; seq1 <- seq(0,25,5)
    ymin2 <- -7; ymax2 <- 15; seq2 <- seq(-5,15,5)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin2 <- -2.5; ymax2 <- 1.5; seq2 <- seq(-2,1,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 5.1; seq1 <- seq(0,5,2.5)
    ymin2 <- -1; ymax2 <- 4.5; seq2 <- seq(-1,4,1)
  }
  
  if(condition=="Sun"){
    subtitle <- "Sun"
  }else if(condition=="Shade"){
    subtitle <- "Shade"
  }

  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = mu1_2.5, ymax = mu1_97.5), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = mu1_50), col = "#FF1493") +
    geom_point(aes(y = data_Mar), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = mu2_2.5, ymax = mu2_97.5), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = mu2_50), col = "#522A17") +
    geom_point(aes(y = data_Sep), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = subtitle,
         x = "",
         #x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance"
         )
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = diff_2.5, ymax = diff_97.5), alpha = 0.5) +
    geom_line(aes(y = diff_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(
      #title = var, subtitle = subtitle, 
         x = "Time relative to initial dawn (h)", 
         y = "Difference in transcript\nabundance")
  
  return(list(g1, g2))
}





##### Visualization of local comparison #####

diff_vis_local <- function(df, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 30; seq1 <- seq(0,30,10)
    ymin2 <- -12; ymax2 <- 10; seq2 <- seq(-12,8,4)
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 5; seq1 <- seq(0,5,1)
    ymin2 <- -2; ymax2 <- 2; seq2 <- seq(-2,2,1)
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- -0.2; ymax1 <- 8; seq1 <- seq(0,8,2)
    ymin2 <- -2.5; ymax2 <- 3; seq2 <- seq(-2,3,1)
  }
  
  g1 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    annotate("rect", xmin = 36, xmax = 39, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = mu1_2.5, ymax = mu1_97.5), alpha = 0.3, fill = "black") +
    geom_line(aes(y = mu1_50), col = "black") +
    geom_point(aes(y = data_A), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = mu2_2.5, ymax = mu2_97.5), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = mu2_50), col = "orangered") +
    geom_point(aes(y = data_W), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = mu3_2.5, ymax = mu3_97.5), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = mu3_50), col = "cyan3") +
    geom_point(aes(y = data_C), col = "cyan3", alpha = 0.6) +
    geom_ribbon(aes(ymin = mu4_2.5, ymax = mu4_97.5), alpha = 0.3, fill = "lavenderblush4") +
    geom_line(aes(y = mu4_50), col = "lavenderblush4") +
    geom_point(aes(y = data_L), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,36,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = "Multiple local treatments", 
         x = "",
         #x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance")
  
  g2 <- ggplot(data = df, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50") +
    annotate("rect", xmin = 36, xmax = 39, ymin = ymin2, ymax = ymax2, alpha = 0.3, fill = "gray50") +
    geom_line(aes(y = diff2_50), col = "orangered") +
    geom_ribbon(aes(ymin = diff2_2.5, ymax = diff2_97.5), alpha = 0.5, fill = "orangered") +
    geom_line(aes(y = diff3_50), col = "cyan3") +
    geom_ribbon(aes(ymin = diff3_2.5, ymax = diff3_97.5), alpha = 0.5, fill = "cyan3") +
    geom_line(aes(y = diff4_50), col = "lavenderblush4") +
    geom_ribbon(aes(ymin = diff4_2.5, ymax = diff4_97.5), alpha = 0.5, fill = "lavenderblush4") +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,36,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq2) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(
         #title = var, subtitle = "Multiple local treatments", 
         x = "Time relative to initial dawn (h)", 
         y = "Difference in transcript\nabundance")
  
  return(list(g1, g2))
}





##### Visualization of SSM prediction #####

Pred_vis <- function(df1, df2, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 20; seq1 <- seq(0,20,5)
    ymin3 <- -0.5; ymax3 <- 1; seq3 <- seq(-0.5,1,0.5)
    ymin4 <- -0.08; ymax4 <- 0.03; seq4 <- seq(-0.06,0.06,0.03) 
    ymin6 <- -5; ymax6 <- 27; seq6 <- seq(0,25,5) 
    tag_local <- "E"
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin3 <- -0.15; ymax3 <- 0.1; seq3 <- seq(-0.1,0.1,0.1)
    ymin4 <- -0.011; ymax4 <- 0.01; seq4 <- seq(-0.01,0.01,0.01)
    ymin5 <- -0.2; ymax5 <- 2; seq5 <- seq(-0,2,1)
    ymin6 <- -0.7; ymax6 <- 4; seq6 <- seq(0,4,1)
    tag_local <- "F"
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 4.5; seq1 <- seq(0,4,2)
    ymin3 <- -0.04; ymax3 <- 0.12; seq3 <- seq(-0.04,0.12,0.04)
    ymin4 <- -0.015; ymax4 <- 0.015; seq4 <- seq(-0.01,0.01,0.01)
    ymin5 <- -0.6; ymax5 <- 1.6; seq5 <- seq(-0.5,1.5,0.5)
    ymin6 <- 0; ymax6 <- 4.2; seq6 <- seq(0,4,2)
    tag_local <- "F"
  }
  
  
  g1 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarSun_2.5, ymax = alpha_MarSun_97.5), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = alpha_MarSun_50), col = "#FF1493") +
    geom_point(aes(y = data_MarSun), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepSun_2.5, ymax = alpha_SepSun_97.5), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = alpha_SepSun_50), col = "#522A17") +
    geom_point(aes(y = data_SepSun), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Sun", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "A")
  
  g2 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarShade_2.5, ymax = alpha_MarShade_97.5), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = alpha_MarShade_50), col = "#FF1493") +
    geom_point(aes(y = data_MarShade), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepShade_2.5, ymax = alpha_SepShade_97.5), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = alpha_SepShade_50), col = "#522A17") +
    geom_point(aes(y = data_SepShade), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Shade", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "B")
  
  g3 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin3, ymax = ymax3, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_temp_2.5, ymax = b_temp_97.5), alpha = 0.5) +
    geom_line(aes(y = b_temp_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq3) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(temperature)",
         tag = "C")
  
  g4 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin4, ymax = ymax4, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_light_2.5, ymax = b_light_97.5), alpha = 0.5) +
    geom_line(aes(y = b_light_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq4) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(irradiance)",
         tag = "D")
  
  g6 <- ggplot(data = df2, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin6, ymax = ymax6, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_A_2.5, ymax = alpha_A_97.5), alpha = 0.3, fill = "black") +
    geom_line(aes(y = alpha_A_50), col = "black") +
    geom_point(aes(y = data_A), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_W_2.5, ymax = alpha_W_97.5), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = alpha_W_50), col = "orangered") +
    geom_point(aes(y = data_W), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_C_2.5, ymax = alpha_C_97.5), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = alpha_C_50), col = "cyan3") +
    geom_point(aes(y = data_C), col = "cyan3", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_L_2.5, ymax = alpha_L_97.5), alpha = 0.3, fill = "lavenderblush4") +
    geom_line(aes(y = alpha_L_50), col = "lavenderblush4") +
    geom_point(aes(y = data_L), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq6) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = "Multiple local treatments", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = tag_local)
  
  if(var=="AhgSIG5"){
    g5 <- ggplot(data = df1, aes(x = time)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_CCA1_2.5, ymax = b_CCA1_97.5), alpha = 0.5) +
      geom_line(aes(y = b_CCA1_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgCCA1), ")", sep="")))))),
           tag = "E")
    
    return(list(g1, g2, g3, g4, g5, g6))
    
  }else if(var=="AhgpsbD BLRP"){
    g5 <- ggplot(data = df1, aes(x = time)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_SIG5_2.5, ymax = b_SIG5_97.5), alpha = 0.5) +
      geom_line(aes(y = b_SIG5_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgSIG5), ")", sep="")))))),
           tag = "E")
    
    return(list(g1, g2, g3, g4, g5, g6))
    
  }else{return(list(g1, g2, g3, g4, g6))}
  
}





##### Figure legend #####

### Sun-Shade
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Condition=c(rep("Sun",5), rep("Shade",5)))
df_legend$Condition = factor(df_legend$Condition, levels=c("Sun", "Shade"))

# plot code
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 10) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("orange", "gray30")) +
  scale_fill_manual(values=c("orange", "#gray30"))

legend_SunShade1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_SunShade2 <- as_ggplot(get_legend(g))



### Mar-Sep
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Season=c(rep("March (Spring)",5), rep("September (Autumn)",5)))
df_legend$Season = factor(df_legend$Season, levels=c("March (Spring)", "September (Autumn)"))

# plot code
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Season, color=Season)) + 
  geom_line(aes(color=Season), size= 1, alpha = 1) +
  theme_classic(base_size = 10) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("#FF1493", "#522A17")) +
  scale_fill_manual(values=c("#FF1493", "#522A17"))

legend_MarSep1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_MarSep2 <- as_ggplot(get_legend(g))



### Local
df_legend <- data.frame(Time=1:10, 
                        Temperature=1:10, 
                        Condition=c(rep("Ambient Conditions",2), rep("Warm",2), rep("Chill",2), rep("Low light",4)))
df_legend$Condition = factor(df_legend$Condition, levels=c("Ambient Conditions", "Warm", "Chill", "Low light"))

# plot code
g <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 10) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.8, 'lines')) +
  scale_colour_manual(values=c("black", "orangered", "cyan3", "lavenderblush4")) +
  scale_fill_manual(values=c("black", "orangered", "cyan3", "lavenderblush4"))

legend_local1 <- as_ggplot(get_legend(g + guides(color = guide_legend(nrow = 1))))
legend_local2 <- as_ggplot(get_legend(g))





##### Visualization of SSM prediction (one season) #####

Pred_vis_one <- function(df1, df2, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -7; ymax1 <- 20; seq1 <- seq(0,20,5)
    ymin3 <- -1.2; ymax3 <- 1.8; seq3 <- seq(-1,1.5,0.5)
    ymin4 <- -0.09; ymax4 <- 0.07; seq4 <- seq(-0.09,0.06,0.03) 
    ymin6 <- -38; ymax6 <- 28; seq6 <- seq(-30,20,10) 
    tag_local <- "E"
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.5; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin3 <- -0.25; ymax3 <- 0.5; seq3 <- seq(-0.2,0.4,0.2)
    ymin4 <- -0.03; ymax4 <- 0.035; seq4 <- seq(-0.03,0.03,0.01)
    ymin5 <- -0.5; ymax5 <- 0.7; seq5 <- seq(-0.5,0.5,0.5)
    ymin6 <- -12; ymax6 <- 14; seq6 <- seq(-10,10,5)
    tag_local <- "F"
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 4.5; seq1 <- seq(0,4,2)
    ymin3 <- -0.04; ymax3 <- 0.12; seq3 <- seq(-0.04,0.12,0.04)
    ymin4 <- -0.015; ymax4 <- 0.015; seq4 <- seq(-0.01,0.01,0.01)
    ymin5 <- -0.4; ymax5 <- 1.2; seq5 <- seq(-0.4,1.2,0.4)
    ymin6 <- 0; ymax6 <- 4.2; seq6 <- seq(0,4,2)
    tag_local <- "F"
  }
  
  
  g1 <- ggplot(data = df1, aes(x = time_Mar)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarSun_2.5, ymax = alpha_MarSun_97.5), alpha = 0.3, fill = "orange") +
    geom_line(aes(y = alpha_MarSun_50), col = "orange") +
    geom_point(aes(y = data_MarSun), col = "orange", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_MarShade_2.5, ymax = alpha_MarShade_97.5), alpha = 0.3, fill = "gray30") +
    geom_line(aes(y = alpha_MarShade_50), col = "gray30") +
    geom_point(aes(y = data_MarShade), col = "gray30", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Sun", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "")

  g2 <- ggplot(data = df1, aes(x = time_Mar)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin3, ymax = ymax3, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_temp_Mar_2.5, ymax = b_temp_Mar_97.5), alpha = 0.5) +
    geom_line(aes(y = b_temp_Mar_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq3) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(temperature)",
         tag = "")
  
  g3 <- ggplot(data = df1, aes(x = time_Mar)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin4, ymax = ymax4, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_light_Mar_2.5, ymax = b_light_Mar_97.5), alpha = 0.5) +
    geom_line(aes(y = b_light_Mar_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq4) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(irradiance)",
         tag = "")
  
  g4 <- ggplot(data = df2, aes(x = time_Mar)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin6, ymax = ymax6, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_A_Mar_2.5, ymax = alpha_A_Mar_97.5), alpha = 0.3, fill = "black") +
    geom_line(aes(y = alpha_A_Mar_50), col = "black") +
    geom_point(aes(y = data_A_Mar), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_W_Mar_2.5, ymax = alpha_W_Mar_97.5), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = alpha_W_Mar_50), col = "orangered") +
    geom_point(aes(y = data_W_Mar), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_C_Mar_2.5, ymax = alpha_C_Mar_97.5), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = alpha_C_Mar_50), col = "cyan3") +
    geom_point(aes(y = data_C_Mar), col = "cyan3", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_L_Mar_2.5, ymax = alpha_L_Mar_97.5), alpha = 0.3, fill = "lavenderblush4") +
    geom_line(aes(y = alpha_L_Mar_50), col = "lavenderblush4") +
    geom_point(aes(y = data_L_Mar), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq6) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = "Multiple local treatments", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "")
  
  g5 <- ggplot(data = df1, aes(x = time_Sep)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_SepSun_2.5, ymax = alpha_SepSun_97.5), alpha = 0.3, fill = "orange") +
    geom_line(aes(y = alpha_SepSun_50), col = "orange") +
    geom_point(aes(y = data_SepSun), col = "orange", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepShade_2.5, ymax = alpha_SepShade_97.5), alpha = 0.3, fill = "gray30") +
    geom_line(aes(y = alpha_SepShade_50), col = "gray30") +
    geom_point(aes(y = data_SepShade), col = "gray30", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Sun", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "")
  
  g6 <- ggplot(data = df1, aes(x = time_Sep)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin3, ymax = ymax3, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_temp_Sep_2.5, ymax = b_temp_Sep_97.5), alpha = 0.5) +
    geom_line(aes(y = b_temp_Sep_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq3) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(temperature)",
         tag = "")
  
  g7 <- ggplot(data = df1, aes(x = time_Sep)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin4, ymax = ymax4, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_light_Sep_2.5, ymax = b_light_Sep_97.5), alpha = 0.5) +
    geom_line(aes(y = b_light_Sep_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq4) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(irradiance)",
         tag = "")
  
  g8 <- ggplot(data = df2, aes(x = time_Sep)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin6, ymax = ymax6, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_A_Sep_2.5, ymax = alpha_A_Sep_97.5), alpha = 0.3, fill = "black") +
    geom_line(aes(y = alpha_A_Sep_50), col = "black") +
    geom_point(aes(y = data_A_Sep), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_W_Sep_2.5, ymax = alpha_W_Sep_97.5), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = alpha_W_Sep_50), col = "orangered") +
    geom_point(aes(y = data_W_Sep), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_C_Sep_2.5, ymax = alpha_C_Sep_97.5), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = alpha_C_Sep_50), col = "cyan3") +
    geom_point(aes(y = data_C_Sep), col = "cyan3", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_L_Sep_2.5, ymax = alpha_L_Sep_97.5), alpha = 0.3, fill = "lavenderblush4") +
    geom_line(aes(y = alpha_L_Sep_50), col = "lavenderblush4") +
    geom_point(aes(y = data_L_Sep), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq6) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = "Multiple local treatments", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance",
         tag = "")
  
  if(var=="AhgSIG5"){
    g9 <- ggplot(data = df1, aes(x = time_Mar)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_CCA1_Mar_2.5, ymax = b_CCA1_Mar_97.5), alpha = 0.5) +
      geom_line(aes(y = b_CCA1_Mar_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgCCA1), ")", sep="")))))),
           tag = "")
    
    g10 <- ggplot(data = df1, aes(x = time_Sep)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_CCA1_Sep_2.5, ymax = b_CCA1_Sep_97.5), alpha = 0.5) +
      geom_line(aes(y = b_CCA1_Sep_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgCCA1), ")", sep="")))))),
           tag = "")
    
    return(list(g1, g2, g3, g9, g4, g5, g6, g7, g10, g8))
    
  }else if(var=="AhgpsbD BLRP"){
    g11 <- ggplot(data = df1, aes(x = time_Mar)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_SIG5_Mar_2.5, ymax = b_SIG5_Mar_97.5), alpha = 0.5) +
      geom_line(aes(y = b_SIG5_Mar_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgSIG5), ")", sep="")))))),
           tag = "")
    
    g12 <- ggplot(data = df1, aes(x = time_Sep)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_SIG5_Sep_2.5, ymax = b_SIG5_Sep_97.5), alpha = 0.5) +
      geom_line(aes(y = b_SIG5_Sep_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgSIG5), ")", sep="")))))),
           tag = "")
    
    return(list(g1, g2, g3, g11, g4, g5, g6, g7, g12, g8))
    
  }else{return(list(g1, g2, g3, g4, g5, g6, g7, g8))}
  
}





##### Visualization of SSM prediction (with one environmental variable) #####

Pred_vis_1env <- function(df1, df2, var){
  
  if(var=="AhgCCA1"){
    ymin1 <- -5; ymax1 <- 20; seq1 <- seq(0,20,5)
    ymin3 <- -0.5; ymax3 <- 1; seq3 <- seq(-0.5,1,0.5)
    ymin4 <- -3; ymax4 <- 1; seq4 <- seq(-3,1,1) 
    ymin6 <- -5; ymax6 <- 27; seq6 <- seq(0,25,5) 
    tag_local <- "E"
  }else if(var=="AhgSIG5"){
    ymin1 <- -0.7; ymax1 <- 3.1; seq1 <- seq(0,3,1)
    ymin3 <- -0.15; ymax3 <- 0.1; seq3 <- seq(-0.1,0.1,0.1)
    ymin4 <- -0.011; ymax4 <- 0.01; seq4 <- seq(-0.01,0.01,0.01)
    ymin5 <- -0.2; ymax5 <- 2; seq5 <- seq(-0,2,1)
    ymin6 <- -0.7; ymax6 <- 4; seq6 <- seq(0,4,1)
    tag_local <- "F"
  }else if(var=="AhgpsbD BLRP"){
    ymin1 <- 0; ymax1 <- 4.5; seq1 <- seq(0,4,2)
    ymin3 <- -0.04; ymax3 <- 0.12; seq3 <- seq(-0.04,0.12,0.04)
    ymin4 <- -0.015; ymax4 <- 0.015; seq4 <- seq(-0.01,0.01,0.01)
    ymin5 <- -0.6; ymax5 <- 1.6; seq5 <- seq(-0.5,1.5,0.5)
    ymin6 <- 0; ymax6 <- 4.2; seq6 <- seq(0,4,2)
    tag_local <- "F"
  }
  
  g0 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = mu_2.5, ymax = mu_97.5), alpha = 0.5) +
    geom_line(aes(y = mu_50)) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, 
         x = "Time relative to initial dawn (h)", 
         y = "Trend of transcript\nabundance") 
  
  g1 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarSun_2.5, ymax = alpha_MarSun_97.5), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = alpha_MarSun_50), col = "#FF1493") +
    geom_point(aes(y = data_MarSun), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepSun_2.5, ymax = alpha_SepSun_97.5), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = alpha_SepSun_50), col = "#522A17") +
    geom_point(aes(y = data_SepSun), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Sun", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance")
  
  g2 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin1, ymax = ymax1, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_MarShade_2.5, ymax = alpha_MarShade_97.5), alpha = 0.3, fill = "#FF1493") +
    geom_line(aes(y = alpha_MarShade_50), col = "#FF1493") +
    geom_point(aes(y = data_MarShade), col = "#FF1493", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_SepShade_2.5, ymax = alpha_SepShade_97.5), alpha = 0.3, fill = "#522A17") +
    geom_line(aes(y = alpha_SepShade_50), col = "#522A17") +
    geom_point(aes(y = data_SepShade), col = "#522A17", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +
    scale_y_continuous(expand = c(0,0), breaks=seq1) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(title = var, subtitle = "Shade", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance")
  
  g3 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin3, ymax = ymax3, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_temp_2.5, ymax = b_temp_97.5), alpha = 0.5) +
    geom_line(aes(y = b_temp_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq3) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(temperature)")
  
  g4 <- ggplot(data = df1, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin4, ymax = ymax4, alpha = 0.3, fill = "gray50")+
    geom_ribbon(aes(ymin = b_light_2.5, ymax = b_light_97.5), alpha = 0.5) +
    geom_line(aes(y = b_light_50)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    scale_x_continuous(breaks=seq(12,30,6)) + 
    scale_y_continuous(expand = c(0,0), breaks=seq4) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          plot.tag = element_text(face = "bold"),
          axis.text = element_text(size = 10)) +
    labs(x = "Time relative to initial dawn (h)", 
         y = "Coefficient of\nregression\n(irradiance)")
  
  g6 <- ggplot(data = df2, aes(x = time)) +
    annotate("rect", xmin = 12, xmax = 24, ymin = ymin6, ymax = ymax6, alpha = 0.3, fill = "gray50") +
    geom_ribbon(aes(ymin = alpha_A_2.5, ymax = alpha_A_97.5), alpha = 0.3, fill = "black") +
    geom_line(aes(y = alpha_A_50), col = "black") +
    geom_point(aes(y = data_A), col = "black", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_W_2.5, ymax = alpha_W_97.5), alpha = 0.3, fill = "orangered") +
    geom_line(aes(y = alpha_W_50), col = "orangered") +
    geom_point(aes(y = data_W), col = "orangered", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_C_2.5, ymax = alpha_C_97.5), alpha = 0.3, fill = "cyan3") +
    geom_line(aes(y = alpha_C_50), col = "cyan3") +
    geom_point(aes(y = data_C), col = "cyan3", alpha = 0.6) +
    geom_ribbon(aes(ymin = alpha_L_2.5, ymax = alpha_L_97.5), alpha = 0.3, fill = "lavenderblush4") +
    geom_line(aes(y = alpha_L_50), col = "lavenderblush4") +
    geom_point(aes(y = data_L), col = "lavenderblush4", alpha = 0.6) +
    scale_x_continuous(breaks=seq(12,30,6)) +  
    scale_y_continuous(expand = c(0,0), breaks=seq6) +
    theme_classic(base_size = 10) +
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          plot.title = element_text(size = 10, face = "italic"),
          plot.tag = element_text(face = "bold")) +
    labs(title = var, subtitle = "Multiple local treatments", 
         x = "Time relative to initial dawn (h)", 
         y = "Relative transcript\nabundance")
  
  if(var=="AhgSIG5"){
    g5 <- ggplot(data = df1, aes(x = time)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_CCA1_2.5, ymax = b_CCA1_97.5), alpha = 0.5) +
      geom_line(aes(y = b_CCA1_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgCCA1), ")", sep="")))))))
    
    return(list(g1, g2, g0, g3, g4, g5, g6))
    
  }else if(var=="AhgpsbD BLRP"){
    g5 <- ggplot(data = df1, aes(x = time)) +
      annotate("rect", xmin = 12, xmax = 24, ymin = ymin5, ymax = ymax5, alpha = 0.3, fill = "gray50")+
      geom_ribbon(aes(ymin = b_SIG5_2.5, ymax = b_SIG5_97.5), alpha = 0.5) +
      geom_line(aes(y = b_SIG5_50)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      scale_x_continuous(breaks=seq(12,30,6)) + 
      scale_y_continuous(expand = c(0,0), breaks=seq5) +
      theme_classic(base_size = 10) +
      theme(legend.position = "none",
            plot.tag = element_text(face = "bold"),
            axis.text = element_text(size = 10)) +
      labs(x = "Time relative to initial dawn (h)", 
           y = expression(atop(scriptscriptstyle(""), atop(textstyle("Coefficient of"), atop(textstyle("regression"), textstyle(paste("(", italic(AhgSIG5), ")", sep="")))))))
    
    return(list(g1, g2, g0, g3, g4, g5, g6))
    
  }else{return(list(g1, g2, g0, g3, g4, g6))}
  
}
