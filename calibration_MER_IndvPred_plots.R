# Load required libraries
source("PLOTS_FUNCTIONS.R")
library(ggplot2)
library(wacolors)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)

files_combs <- crossing(
  "ID" = 3,
  "TS"=0.25,
  "DS"=4
)
print(nrow(files_combs))

process_model <- function(id, ts, ds) {

    ind_pred_list <- list()
    cal_plot_list <- list()
    MER_list <- list()
    foreach (model = 1:42) %dopar% {
      model_name <- 0
      for(it in 1:200) {
        ind_filename <- paste0("raw_results/preds/indv_preds_results/indv_preds_results_ID", id, "_IT", it, "_TS", ts, "_DS", ds, ".csv")
        cal_filename <- paste0("raw_results/cal_plot/cal_plot_results/cal_plot_results_ID", id, "_IT", it, "_TS", ts, "_DS", ds, ".rds")
        mer_filename <- paste0("raw_results/validation/validation_results_ID", id, "_IT", it, "_TS", ts, "_DS", ds, ".csv")
        
        ind_pred_df <- read.csv(ind_filename)
        ind_pred_list[[it]] <- ind_pred_df[model, ]
        
        cal_plot_list_df <- readRDS(cal_filename)
        cal_plot_list[[it]] <- cal_plot_list_df[model, ]
        
        mer_df <- read.csv(mer_filename)
        MER_list[[it]] <- mer_df[model, ]$MER
        model_name <- mer_df[model, ]$Model
      }
      scenario_name <- paste0("SC_ID", id, "_TS", ts, "_DS", ds)
      # Process results or generate plots here
      calibration_instability_plot_V3(cal_plot_list, scenario_name)
      indv_stability_plot(ind_pred_list, scenario_name)
      MER_plot(MER_list, model_name, scenario_name)
    }
}





# Apply the function to each combination of files
for(i in 1:nrow(files_combs)){ #:nrow(files_combs))
  process_model(files_combs[i, ]$ID, files_combs[i, ]$TS, files_combs[i, ]$DS)
}

