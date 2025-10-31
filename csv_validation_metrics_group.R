
# Load required libraries
start.time <- Sys.time()
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
      scenario_name <- paste0("SC_ID",id,"_TS",ts, "_DS",ds)
      val_metric_model15 <- data.frame()
      val_metric_model5 <- data.frame()
      val_metric_model4 <- data.frame()
      
      
      for(it in 1:200){#1:200
        val_filename <- paste0("../raw_results/validation/validation_results_ID",id,"_IT", it, "_TS",ts, "_DS",ds,".csv")
        val_df <- read.csv(val_filename)
        val_metric_model15 <- rbind(val_metric_model15,  subset(val_df, grepl("model15", Model)))
        val_metric_model5 <- rbind(val_metric_model5,  subset(val_df, grepl("model5", Model)))
        val_metric_model4 <- rbind(val_metric_model4,  subset(val_df, grepl("model4", Model)))
      }
      
      val_data_model15 <- get_val_data(val_metric_model15)
      val_data_model5 <- get_val_data(val_metric_model5)
      val_data_model4 <- get_val_data(val_metric_model4)
      
      if (!dir.exists("csv_validation_results/")) {
        dir.create("csv_validation_results/", recursive = TRUE)
      }
      
      write.csv(val_data_model15, paste0("../csv_validation_results/validation_mterics_", scenario_name, "_", 'model15', ".csv"))
      write.csv(val_data_model5, paste0("../csv_validation_results/validation_mterics_", scenario_name, "_", 'model5', ".csv"))
      write.csv(val_data_model4, paste0("../csv_validation_results/validation_mterics_", scenario_name, "_", 'model4', ".csv"))
}



# Apply the function to each combination of files
for(i in 1:nrow(files_combs)){ #:nrow(files_combs))
  process_model(files_combs[i, ]$ID, files_combs[i, ]$TS, files_combs[i, ]$DS)
}
