source("PLOTS_FUNCTIONS.R")
library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

files_combs <- crossing(
  "ID" = 1,
  "TS"=0.25,
  "DS"=4
)
#val metrics for one scenario (per ID, TS, DS), 200 iterations. 3 plots, one for each dataset
for(ro in 1:1){
  id <- files_combs[ro,]$ID
  ts <- files_combs[ro,]$TS
  ds <- files_combs[ro,]$DS
  
  scenario_name <- paste0("SC_ID",id,"_TS",ts, "_DS",ds)
  val_metric_model15 <- data.frame()
  val_metric_model5 <- data.frame()
  val_metric_model4 <- data.frame()
  
  
  for(it in 1:200){#1:200
    val_filename <- paste0("../raw_results_csf/validation/validation_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".csv")
    val_df <- read.csv(val_filename)
    val_metric_model15 <- rbind(val_metric_model15,  subset(val_df, grepl("model15", Model)))
    val_metric_model5 <- rbind(val_metric_model5,  subset(val_df, grepl("model5", Model)))
    val_metric_model4 <- rbind(val_metric_model4,  subset(val_df, grepl("model4", Model)))
  }
  
  val_data_model15 <- get_val_data(val_metric_model15)
  val_data_model5 <- get_val_data(val_metric_model5)
  val_data_model4 <- get_val_data(val_metric_model4)
  
  val_metric_plot(val_data_model15, scenario_name, "model15")
  val_metric_plot(val_data_model5, scenario_name, "model5")
  val_metric_plot(val_data_model4, scenario_name, "model4")
}

for(ro in 1:1){
  id <- files_combs[ro,]$ID
  ts <- files_combs[ro,]$TS
  ds <- files_combs[ro,]$DS
  
  ind_pred_list <- list()
  cal_plot_list <- list()
  MER_list <- list()
  for(model in 1:42){
    model_name <- 0
    for(it in 1:200){#1:200
      ind_filename <- paste0("../raw_results_csf/preds/indv_preds_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".csv")
      cal_filename <- paste0("../raw_results_csf/cal_plot_results/cal_plot_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".rds") #cal_plot_results_ID
      mer_filename <- paste0("../raw_results_csf/validation/validation_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".csv")
      
      
      ind_pred_df <- read.csv(ind_filename)
      ind_pred_list[[it]] <- ind_pred_df[model,]
      
      cal_plot_list_df <- readRDS(cal_filename)
      cal_plot_list[[it]] <- cal_plot_list_df[model,]
      
      mer_df <- read.csv(mer_filename)
      MER_list[[it]] <- mer_df[model,]$MER
      model_name <- mer_df[model,]$Model
      
    }
    scenario_name <- paste0("SC_ID",id,"_TS",ts, "_DS",ds)
    indv_stability_plot(ind_pred_list, scenario_name)
    calibration_instability_plot(cal_plot_list, scenario_name)
    MER_plot(MER_list, model_name, scenario_name)
  }
}



for(ro in 1:1){
  
  
  id <- files_combs[ro,]$ID
  ts <- files_combs[ro,]$TS
  ds <- files_combs[ro,]$DS
  
  
  scenario_name <- paste0("SC_ID",id,"_TS",ts, "_DS",ds)
  mean_cal_plot_model15 <- list()
  mean_cal_plot_model5 <- list()
  mean_cal_plot_model4 <- list()
  
  
  for(it in 1:200){#1:200
    cal_filename <- paste0("../raw_results_csf/cal_plot_results/cal_plot_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".rds") #cal_plot_results_ID
    cal_df <- readRDS(cal_filename)
    mean_cal_plot_model15[[it]] <-   subset(cal_df, grepl("model15", Model))
    mean_cal_plot_model5[[it]] <-   subset(cal_df, grepl("model5", Model))
    mean_cal_plot_model4[[it]] <-  subset(cal_df, grepl("model4", Model))
  }
  
  
  smoothed_mean_list_model15<-list()
  smoothed_mean_list_model5<-list()
  smoothed_mean_list_model4<-list()
  for(model in 1:14){
    model_name <- 0
    cal_plot_list_model15 <- list()
    cal_plot_list_model5 <- list()
    cal_plot_list_model4 <- list()
    
    
    for(it in 1:200){#1:200
      cal_plot_list_model15[[it]] <- mean_cal_plot_model15[[it]][model,]
      cal_plot_list_model5[[it]] <- mean_cal_plot_model5[[it]][model,]
      cal_plot_list_model4[[it]] <- mean_cal_plot_model4[[it]][model,]
    }
    scenario_name <- paste0("SC_ID",id,"_TS",ts, "_DS",ds)
    smoothed_mean_list_model15[[model]]<-get_smoothed_mean(cal_plot_list_model15, scenario_name)
    smoothed_mean_list_model5[[model]]<-get_smoothed_mean(cal_plot_list_model5, scenario_name)
    smoothed_mean_list_model4[[model]]<-get_smoothed_mean(cal_plot_list_model4, scenario_name)
    
  }
  
  group_mean_cal_plots(smoothed_mean_list_model15, 'model15')
  group_mean_cal_plots(smoothed_mean_list_model5, 'model5')
  group_mean_cal_plots(smoothed_mean_list_model4, 'model4')
  
  
}