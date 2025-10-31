library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

df <- read.csv("../csv_validation_results/validation_mterics_SC_ID1_TS0.25_DS4_model4.csv")
filename<- "numpredictors_ID1_TS0.25_DS4_model4_R2.pdf"


df <- df %>% dplyr::filter(!grepl("corrW|noLambda|mah|addY|bayesian", Model, ignore.case = TRUE))
print(df)

df$Model[df$Model == "propensity_model_missW_recalibrate_model4"] <- "Membership-based recalibration"

df$Model[df$Model == "target_only_model_model4"]<-"Target-only"
df$Model[df$Model == "intercept_calibration_source_only_model4"]<-"Intercept Recalibration-ancillary only"
df$Model[df$Model == "intercept_calibration_full_model4"]<-"Intercept Recalibration-all data" 
df$Model[df$Model == "logistic_calibration_full_model4"]<-"Logistic Recalibration-all data"
df$Model[df$Model == "logistic_calibration_source_only_model4"]<-"Logistic Recalibration-ancillary only"

df$Model <- factor(df$Model, levels = c("Membership-based recalibration",
                                        "Intercept Recalibration-ancillary only",
                                        "Intercept Recalibration-all data" ,
                                        "Logistic Recalibration-ancillary only",
                                        "Logistic Recalibration-all data",
                                        "Target-only"
))
labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
legends <- list()
figs <- list()
plots <- 0

scaleFUN <- function(x) sprintf("%.3f", x)

for(i in 1:4){
  data <- df %>% dplyr::select(matches(paste0("Model|", labels[i])))
  print(data)
  colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
  
  if(i == 1 ){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +   theme(
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank()  # Remove background for facets if any
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+scale_x_continuous(breaks = c( 0.70, 0.74, 0.78, 0.80), limits = c(0.70 , 0.80))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i == 2){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks = c(-0.3,0,0.3), limits = c(-0.3 , 0.4))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i == 3){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +    
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks = c(0.7,0.9,1.1, 1.3), limits = c(0.69 , 1.5))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i==4){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+  scale_x_continuous(breaks = c(0.07,0.08,0.09), limits = c(0.07 , 0.095))
  }
}


pdf(filename, width =8, height = 2.5)
combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1, widths=c(2.4,1,1,0.8)
)

show(combined_plot)

dev.off()




##########

df <- read.csv("../csv_validation_results/validation_mterics_SC_ID1_TS0.25_DS4_model5.csv")
filename<- "numpredictors_ID1_TS0.25_DS4_model5_R2.pdf"


df <- df %>% dplyr::filter(!grepl("corrW|noLambda|mah|addY|bayesian", Model, ignore.case = TRUE))
print(df)

df$Model[df$Model == "propensity_model_missW_recalibrate_model5"] <- "Membership-based recalibration"

df$Model[df$Model == "target_only_model_model5"]<-"Target-only"
df$Model[df$Model == "intercept_calibration_source_only_model5"]<-"Intercept Recalibration-ancillary only"
df$Model[df$Model == "intercept_calibration_full_model5"]<-"Intercept Recalibration-all data" 
df$Model[df$Model == "logistic_calibration_full_model5"]<-"Logistic Recalibration-all data"
df$Model[df$Model == "logistic_calibration_source_only_model5"]<-"Logistic Recalibration-ancillary only"

df$Model <- factor(df$Model, levels = c("Membership-based recalibration",
                                        "Intercept Recalibration-ancillary only",
                                        "Intercept Recalibration-all data" ,
                                        "Logistic Recalibration-ancillary only",
                                        "Logistic Recalibration-all data",
                                        "Target-only"
))
labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
legends <- list()
figs <- list()
plots <- 0

scaleFUN <- function(x) sprintf("%.3f", x)

for(i in 1:4){
  data <- df %>% dplyr::select(matches(paste0("Model|", labels[i])))
  print(data)
  colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
  
  if(i == 1 ){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +   theme(
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank()  # Remove background for facets if any
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+scale_x_continuous(breaks = c( 0.70, 0.74, 0.78, 0.80), limits = c(0.70 , 0.80))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i == 2){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks =  c(-0.3,0,0.3), limits = c(-0.3 , 0.4))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i == 3){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +    
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks = c(0.7,0.9,1.1, 1.3), limits = c(0.69 , 1.5))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i==4){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+  scale_x_continuous(breaks = c(0.07,0.08,0.09), limits = c(0.07 , 0.095))
  }
}


pdf(filename, width =8, height = 2.5)
combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1, widths=c(2.4,1,1,0.8)
)

show(combined_plot)

dev.off()



#######################


df <- read.csv("../csv_validation_results/validation_mterics_SC_ID1_TS0.25_DS4_model15.csv")
filename<- "numpredictors_ID1_TS0.25_DS4_model15_R2.pdf"


df <- df %>% dplyr::filter(!grepl("corrW|noLambda|mah|addY|bayesian", Model, ignore.case = TRUE))
print(df)

df$Model[df$Model == "propensity_model_missW_recalibrate_model15"] <- "Membership-based recalibration"

df$Model[df$Model == "target_only_model_model15"]<-"Target-only"
df$Model[df$Model == "intercept_calibration_source_only_model15"]<-"Intercept Recalibration-ancillary only"
df$Model[df$Model == "intercept_calibration_full_model15"]<-"Intercept Recalibration-all data" 
df$Model[df$Model == "logistic_calibration_full_model15"]<-"Logistic Recalibration-all data"
df$Model[df$Model == "logistic_calibration_source_only_model15"]<-"Logistic Recalibration-ancillary only"

df$Model <- factor(df$Model, levels = c("Membership-based recalibration",
                                        "Intercept Recalibration-ancillary only",
                                        "Intercept Recalibration-all data" ,
                                        "Logistic Recalibration-ancillary only",
                                        "Logistic Recalibration-all data",
                                        "Target-only"
))
labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
legends <- list()
figs <- list()
plots <- 0

scaleFUN <- function(x) sprintf("%.3f", x)

for(i in 1:4){
  data <- df %>% dplyr::select(matches(paste0("Model|", labels[i])))
  print(data)
  colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
  
  if(i == 1 ){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +   theme(
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank()  # Remove background for facets if any
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+scale_x_continuous(breaks = c( 0.70, 0.74, 0.78, 0.80), limits = c(0.70 , 0.80))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i == 2){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks = c(-0.3,0,0.3), limits = c(-0.3 , 0.4))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i == 3){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +    
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") +scale_x_continuous(breaks = c(0.7,0.9,1.1, 1.3), limits = c(0.69 , 1.5))#+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
  }
  if(i==4){
    figs[[i]] <- ggplot(data, aes(y=Model, x=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", linewidth = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", linewidth = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+  scale_x_continuous(breaks = c(0.07,0.08,0.09), limits = c(0.07 , 0.095))
  }
}


pdf(filename, width =8, height = 2.5)
combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1, widths=c(2.4,1,1,0.8)
)

show(combined_plot)

dev.off()











