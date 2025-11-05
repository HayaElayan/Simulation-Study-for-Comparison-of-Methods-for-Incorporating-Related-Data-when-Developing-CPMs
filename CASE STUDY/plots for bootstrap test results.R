library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

boot_test <- read.csv("boot_test_ci.csv")

boot_test<- boot_test[boot_test$model_name != "Membership-based weighted model (weights limited to 1)" & boot_test$model_name != "Naive Logistic-developed on full data",]
boot_test

boot_test$model_name[boot_test$model_name == "Membership-based weighted model (weights limited to 1 + Forgetting factor)"] <- "Membership-based recalibration"

boot_test$model_name[boot_test$model_name == "Naive Logistic-developed on target only"]<-"Target-only"
boot_test$model_name[boot_test$model_name == "Intercept recalibration-developed on source only"]<-"Intercept Recalibration-ancillary only"
boot_test$model_name[boot_test$model_name == "Intercept recalibration-developed on full data"]<-"Intercept Recalibration-all data" 
boot_test$model_name[boot_test$model_name == "Logistic recalibration-developed on full data"]<-"Logistic Recalibration-all data"
boot_test$model_name[boot_test$model_name == "Logistic recalibration-developed on source only"]<-"Logistic Recalibration-ancillary only"

boot_test$model_name <- factor(boot_test$model_name, levels = c("Membership-based recalibration",
                                                                          "Intercept Recalibration-ancillary only",
                                                                          "Intercept Recalibration-all data" ,
                                                                          "Logistic Recalibration-ancillary only",
                                                                          "Logistic Recalibration-all data",
                                                                          "Target-only"
))

labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
labels2 <- c('Test AUC', 'Test CITL', 'Test CSLOPE', 'Test BrierScore')
legends <- list()
figs <- list()
plots <- 0


scaleFUN <- function(x) sprintf("%.4f", x)

for(i in 1:4){
  data <- boot_test[boot_test$statistics==labels[i],]
  print(data)
  
  if(i==1){
    figs[[i]] <- ggplot(data, aes(y=model_name, x=mean, colour=model_name, shape=model_name)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme( axis.text.y = element_text(size = 7),
             axis.title.y = element_blank(),
             axis.title.x = element_text(size = 9),
             legend.position = 'none',  # Remove legend
             panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
             panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
             panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
             strip.background = element_blank() 
      )+ labs(x= labels2[i], colour="model_name", shape="model_name"
      )+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i==2){
    figs[[i]] <-ggplot(data, aes(y=model_name, x=mean, colour=model_name, shape=model_name)) +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(     axis.ticks.y = element_blank(), axis.text.y =element_blank(),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size = 9),
                 legend.position = 'none',  # Remove legend
                 panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
                 panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
                 panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
                 strip.background = element_blank() 
      )+ labs(x= labels2[i], colour="model_name", shape="model_name"
      )+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i==3){
    figs[[i]] <-ggplot(data, aes(y=model_name, x=mean, colour=model_name, shape=model_name)) +  
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.25) + # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme(     axis.ticks.y = element_blank(), axis.text.y =element_blank(),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size = 9),
                 legend.position = 'none',  # Remove legend
                 panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
                 panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
                 panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
                 strip.background = element_blank() 
      )+ labs(x= labels2[i], colour="model_name", shape="model_name"
      )+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  if(i==4){
    figs[[i]] <-ggplot(data, aes(y=model_name, x=mean, colour=model_name, shape=model_name)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(xmin =lower_band, xmax =upper_band), width = 0
      )  + geom_point(size=2) +
      theme( 
        axis.ticks.y = element_blank(), axis.text.y =element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels2[i], colour="model_name", shape="model_name"
      ) + scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
  
}

filename<- "V2_bootstrap_test_ci.pdf"

pdf(filename, width =8, height = 2.5)
combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1, widths=c(2,0.95,0.95,0.95)
)

show(combined_plot)

dev.off()


