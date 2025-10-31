library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

df <- read.csv("../csv_validation_results/validation_mterics_SC_ID103_TS0.25_DS4_model15.csv")
filename<- "specification_combnationshift_ID103_TS0.25_DS4_model15.pdf"

df <- df %>% dplyr::filter(!grepl("intercept|target_only|logistic|bayesian|noLambda|mah", Model, ignore.case = TRUE))
print(df)

df$Model[df$Model == "propensity_model_missW_recalibrate_model15"] <- "Membership-based recalibration \n(weightes misspecified)"
df$Model[df$Model == "propensity_model_missW_addY_model15"]<-"Membership-based add-Y \n(weightes misspecified)"

df$Model[df$Model == "propensity_model_corrW_recalibrate_model15"] <- "Membership-based recalibration \n(weightes correctly specified)"
df$Model[df$Model == "propensity_model_corrW_addY_model15"]<-"Membership-based add-Y \n(weightes correctly specified)"


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
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = 'none',  # Remove legend
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with black borders
        panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
        strip.background = element_blank()  # Remove background for facets if any
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
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
        panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model") + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
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
        panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
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
        panel.grid.major = element_line(color = "gray", size = 0.07),  # Light gray grid lines
        panel.grid.minor = element_line(color = "gray", size = 0.07),  # Minor grid lines
        strip.background = element_blank() 
      )+ labs(x= labels[i], colour="Model", shape="Model"
      )+ scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  }
}


pdf(filename, width =8, height = 3)
combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1, widths=c(2,1,1,0.95)
)

show(combined_plot)

dev.off()



































library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

df <- read.csv("validation_mterics_SC_ID103_TS0.25_DS4_model15.csv")
filename<- "specification_combnationshift_ID103_TS0.25_DS4_model15.pdf"

df <- df %>% dplyr::filter(!grepl("intercept|target_only|logistic|bayesian|noLambda", Model, ignore.case = TRUE))
print(df)

df$Model[df$Model == "propensity_model_missW_recalibrate_model15"] <- "Membership-based recalibration (weightes misspecified)"
df$Model[df$Model == "propensity_model_missW_addY_model15"]<-"Membership-based add-Y (weightes misspecified)"

df$Model[df$Model == "propensity_model_corrW_recalibrate_model15"] <- "Membership-based recalibration (weightes correctly specified)"
df$Model[df$Model == "propensity_model_corrW_addY_model15"]<-"Membership-based add-Y (weightes correctly specified)"


df$Model[df$Model == "mah_model_missW_recalibrate_model15"]<-"Distance-based recalibration (weightes misspecified)"
df$Model[df$Model == "mah_model_missW_addY_model15"]<-"Distance-based add-Y (weightes misspecified)"

df$Model[df$Model == "mah_model_corrW_recalibrate_model15"]<-"Distance-based recalibration (weightes correctly specified)"
df$Model[df$Model == "mah_model_corrW_addY_model15"]<-"Distance-based add-Y (weightes correctly specified)"




# df$Model[df$Model == "target_only_model_model15"]<-"Target-only"
# df$Model[df$Model == "intercept_calibration_source_only_model15"]<-"Intercept Recalibration-source only"
# df$Model[df$Model == "intercept_calibration_full_model15"]<-"Intercept Recalibration-all data" 
# df$Model[df$Model == "logistic_calibration_full_model15"]<-"Logistic Recalibration-all data"
# df$Model[df$Model == "logistic_calibration_source_only_model15"]<-"Logistic Recalibration-source only"
# df$Model[df$Model == "bayesian_updating_model15"]<-"Bayesian"

# df$Model <- factor(df$Model, levels = c("Membership-based recalibration",
#                                         "Membership-based add-Y",
#                                         "Distance-based recalibration",
#                                         "Distance-based add-Y",
#                                         "Intercept Recalibration-source only",
#                                         "Intercept Recalibration-all data" ,
#                                         "Logistic Recalibration-source only",
#                                         "Logistic Recalibration-all data",
#                                         "Target-only",
#                                         "Bayesian"
#))
labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
legends <- list()
figs <- list()
plots <- 0

for(i in 1:4){
  data <- df %>% dplyr::select(matches(paste0("Model|", labels[i])))
  print(data)
  colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
  if(i==1){
    figs[[i]] <- ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(ymin =round(lower_band,3), ymax =round(upper_band,3)), width = 0
      )  +  scale_shape_manual(values=seq(1,10))+ geom_point(size=2) +
      ylim(round(min(pmin(data$lower_band,data$Median)),3), round(max(pmax(data$upper_band,data$Median)),3))  +
      theme(axis.title.x =element_blank(), axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.title.y=element_text(size=9),
            legend.position='none'  
      )+ labs(y= labels[i], colour="Model", shape="Model"
      )
  }else{
    figs[[i]] <- ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(ymin = lower_band , ymax = upper_band), width = 0
      )  +  scale_shape_manual(values=seq(1,8))+ geom_point(size=2) +
      ylim(min(pmin(data$lower_band,data$Median)), max(pmax(data$upper_band,data$Median)))  +
      theme(axis.title.x =element_blank(), axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.title.y=element_text(size=9),
            legend.position='none'  
      )+ labs(y= labels[i], colour="Model", shape="Model"
      )
  }
  plots<-  ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
    scale_shape_manual(values=seq(1,8))+ geom_point(size=2) +
    theme( legend.text=element_text(size=7), legend.title=element_text(size=9), legend.key.size = unit(0.5, "cm")
    ) + labs(y= labels[i], colour="Model", shape="Model")+guides(shape = guide_legend(ncol=2))
  
}


combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 2, nrow = 2)
#legend <- get_legend(plots)$grob[[1]]$grob  # Get legend from any plot
grobs <- ggplotGrob(plots)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

pdf(filename, width =6, height = 4.5)

plot_grid(combined_plot, legend,  nrow=2,rel_widths = c(1, 1), rel_heights = c(1,0.3))  
show(combined_plot)


dev.off()


