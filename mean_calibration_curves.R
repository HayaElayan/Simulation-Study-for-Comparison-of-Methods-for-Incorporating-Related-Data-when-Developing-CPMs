library(ggplot2)
require(wacolors)
library(tidyr)
library(dplyr)
require(stringr)
library(gridExtra)
library(cowplot)
library(patchwork)



# Create combinations of files
files_combs_ID = c(1, 97, 5, 3, 103)

sc_names <- c("(a) No Shift", "(b) Low degree case-mix", "(c) Event rate shift", 
              "(d) Predictor-outcome association shift",
              "(e) Combination of Single Case-mix, Event rate and\n Predictor-outcome association shifts")

group_mean_cal_plots_ggplot <- function(data2, dataset_name, title) {
  # Define the output filename
  #filename <- paste0("comb_results/grouped_cal/6Models_mean_cal_plot_", data2[[1]]$sc_name, "_", dataset_name, ".pdf")
  
  # Process and clean data for plotting
  data <- list()
  count <- 1
  for(i in seq_along(data2)){
    if(!grepl("corrW|mah|bayesian|addY", data2[[i]]$model_name, ignore.case = TRUE)){
      if(grepl("propensity_model_missW_recalibrate", data2[[i]]$model_name, ignore.case = TRUE)){
        data2[[i]]$model_name <- "Membership-based recalibration"
      }
      #if(grepl("propensity_model_missW_addY", data2[[i]]$model_name, ignore.case = TRUE)){
      #	data2[[i]]$model_name = "Membership-based add-Y"
      #}
      #if(grepl("mah_model_missW_recalibrate", data2[[i]]$model_name, ignore.case = TRUE) ){
      #	data2[[i]]$model_name = "Distance-based recalibration"
      #}
      #if(grepl("mah_model_missW_addY", data2[[i]]$model_name, ignore.case = TRUE) ){
      #	data2[[i]]$model_name = "Distance-based add-Y"
      #}
      if(grepl("target_only_model", data2[[i]]$model_name, ignore.case = TRUE) ){
        data2[[i]]$model_name <- "Target-only"
      }
      if(grepl("intercept_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
        data2[[i]]$model_name <- "Intercept Recalibration-ancillary only"
      }
      if(grepl("intercept_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
        data2[[i]]$model_name <- "Intercept Recalibration-all data"
      }
      if(grepl("logistic_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
        data2[[i]]$model_name <- "Logistic Recalibration-all data"
      }
      if(grepl("logistic_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
        data2[[i]]$model_name <- "Logistic Recalibration-ancillary only"
      }
      #if(grepl("bayesian_updating", data2[[i]]$model_name, ignore.case = TRUE) ){
      #	data2[[i]]$model_name = "Bayesian"
      #}
      data[[count]] <- data2[[i]]
      count <- count+1
    }}
  
  # Combine data into a data frame for ggplot
  plot_data <- bind_rows(lapply(data, function(d) {
    data.frame(
      Predicted = seq(0.001, 0.99, length = 100),
      Observed = d$smoothed_mean,
      ModelName = d$model_name
    )
  }))
  
  plot_data$ModelName <- factor(plot_data$ModelName, levels = c("Membership-based recalibration",
                                                                #"Membership-based add-Y",
                                                                #"Distance-based recalibration",
                                                                #"Distance-based add-Y",
                                                                "Intercept Recalibration-ancillary only",
                                                                "Intercept Recalibration-all data" ,
                                                                "Logistic Recalibration-ancillary only",
                                                                "Logistic Recalibration-all data",
                                                                "Target-only"
                                                                #"Bayesian"
  ))
  #print(plot_data)
  # Define the plot
  pp<-ggplot(plot_data, aes(x = Predicted, y = Observed, color = ModelName, linetype=ModelName)) +
    geom_line() +
    scale_linetype_manual(values = c("dashed","dotted", "dotdash", "longdash",  "twodash", "dotted")) +
    scale_linewidth_manual(values = c(rep(1, 5), 1.6)) +   # 6th line thicker
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "black") +
    theme_minimal() +
    labs(x = "Predicted", y = "Observed", color = "Model", linetype="Model",  caption = title) +
    theme(
      legend.position = "bottom", 
      legend.text = element_text(size = 8), 
      legend.title=element_text(size=9,face="bold", hjust = 0.5, angle=90) , 
      legend.key.size = unit(2, "cm"), 
      legend.key.width = unit(3.5, "cm"),
      legend.key.height = unit(1.2, "cm"),
      plot.caption = element_text(hjust=0, size=10, face="bold")
    )+guides(
      linetype=guide_legend(ncol=2), 
      color=guide_legend(ncol=2)
    )
  
  # Save the plot to a PDF
  return(pp)
}


get_smoothed_mean <- function(pp_sm_list){
  smoothed_lines <- list()
  
  model_name <- pp_sm_list[[1]]['Model']
  
  for(i in 1:length(pp_sm_list)){
    
    Sm.full  <-  pp_sm_list[[i]]$sm[[1]]
    
    smoothed_lines[[i]] <- Sm.full
  }
  
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- apply(smoothed_matrix, 1, median, na.rm = TRUE) #rowMeans(smoothed_matrix, na.rm = TRUE)
  
  return(list(smoothed_mean=smoothed_mean, model_name=model_name))
}


process_model <- function(id, ts, ds, i, title){
  #scenario_name <- paste0("SC_ID", id, "_TS", ts, "_DS", ds)
  mean_cal_plot_model15 <- list()
  
  for(it in 1:200) {
    cal_filename <- paste0("raw_results/cal_plot/cal_plot_results_ID", id, "_IT", it, "_TS", ts, "_DS", ds, ".rds")
    cal_df <- readRDS(cal_filename)
    mean_cal_plot_model15[[it]] <- subset(cal_df, grepl("model15", Model))
  }
  
  smoothed_mean_list_model15 <- list()
  for(model in 1:14) {
    cal_plot_list_model15 <- list()
    
    for(it in 1:200) {
      cal_plot_list_model15[[it]] <- mean_cal_plot_model15[[it]][model,]
    }
    smoothed_mean_list_model15[[model]] <- get_smoothed_mean(cal_plot_list_model15)
  }
  
  plot <- group_mean_cal_plots_ggplot(smoothed_mean_list_model15, 'model15', title)  # Pass title here
  return(plot)
}



all_plots <- list()

for(i in 1:5) {
  all_plots[[i]] <- process_model(files_combs_ID[i], 0.25, 4, i, sc_names[i]) +
    theme(legend.position = "none")  # Hide legends
}

# Arrange plots (without legends)
figure <- plot_grid(plotlist = all_plots, ncol = 2, nrow = 3, align = "hv", 
                    rel_widths = c(1, 1),  # Adjust width (relative)
                    rel_heights = c(1.4, 1.4, 1.4))  # Adjust height (relative)


# Extract legend from the first plot
legend <- get_legend(
  process_model(files_combs_ID[1], 0.25, 4, 1, sc_names[1]) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),  # Increase text size
      legend.title = element_text(size = 10, face = "bold"),  # Increase title size
      legend.key.size = unit(1, "cm")  # Make legend keys larger
    )
)

# Combine figure with a single legend
final_plot <- plot_grid(figure, legend, ncol = 1, rel_heights = c(1.2, 0.3))


ggsave("one_plot_mean_cal_instability.pdf", plot = final_plot, device = "pdf", width = 8, height = 9)