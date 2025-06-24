#instability in mean estimated risk
MER_plot <- function(MER_list, sc_name){
  	filename <- paste0("comb_results/paper_group_mean_est_risk_", sc_name, "_model15.pdf")
  
	# Define colors and shapes
	legend_colors <- c("#465177",  "#BBB55A","#E2C530", "#D79B1C", "#A96023",
		           "#6D402D", "#393F37", "#2A5638", "#6F4A8E", "#FF6347") 
	legend_shapes <- c("solid", "dashed", "dotted", 
                  "dotdash", "longdash", "twodash", "1F", 
                  "F1", "4C88C488", "12345678")

	# Calculate density limits for all MER_list elements
	densities <- lapply(MER_list, function(x) density(x, na.rm=TRUE))
	xlim <- range(sapply(densities, function(d) d$x))  # Find range of x-axis
	ylim <- range(sapply(densities, function(d) d$y))  # Find range of y-axis

	print(sc_name)
	print(densities)

	# Start plotting
	#pdf(file = filename)

	#plot(densities[[1]], xlab="Mean estimated risk", ylab="", main="", 
	   #  col = legend_colors[1], lty=legend_shapes[1], xlim = xlim, ylim = ylim)

	# Add lines for remaining densities
	#for (i in 2:length(MER_list)) {
	#  lines(densities[[i]], col = legend_colors[i], lty = legend_shapes[i])
	#}
	# Correcting legend reference and adding it



	# Position the legend manually outside of the plot, on the right
	#legend("topleft", 
	#       legend = names(MER_list),  # Adjust this for correct legend names
	 #      col = legend_colors, 
	#       lwd = 2.5, 
	 #      lty = legend_shapes, 
	 #      cex = 0.7, 
	#       bty = "n",  # No border around the legend
	  #     xpd = TRUE)
	#dev.off()

}






MER_plot_ggplot <- function(MER_list, sc_name) {
  # Output filename
  filename <- paste0("comb_results/mer/6Models_group_mean_est_risk_", sc_name, "_model15.pdf")
  
  # Define colors and shapes
  legend_colors <- c("#465177",  "#BBB55A", "#E2C530", "#D79B1C", "#A96023",
                     "#6D402D")#, "#393F37", "#2A5638", "#6F4A8E", "#FF6347")
  legend_linetypes <-  c("solid", "dashed", "dotted", 
                  "dotdash", "longdash", "twodash") #"1F", 
                  #"F1", "4C88C488", "12345678")
  
  # Prepare a data frame for ggplot
  densities <- lapply(MER_list, function(x) density(x, na.rm = TRUE))
  density_df <- do.call(rbind, lapply(seq_along(densities), function(i) {
    data.frame(x = densities[[i]]$x,
               y = densities[[i]]$y,
               Model = names(MER_list)[i])
  }))
  
  # Create the ggplot
  plot <- ggplot(density_df, aes(x = x, y = y,color = Model, linetype = Model)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = legend_linetypes) +
    labs(x = "Mean estimated risk", y = "", title = "", color="Model", linetype="Model") +
    theme_minimal() +
    theme(legend.position = "right", legend.title = element_text(size = 8), legend.text = element_text(size = 7))
  
  # Save the plot to a file
  ggsave(filename, plot = plot, device = "pdf", width = 8, height = 6)
}


get_smoothed_mean <- function(pp_sm_list, sc_name){
  smoothed_lines <- list()
  
  model_name <- pp_sm_list[[1]]['Model']

  for(i in 1:length(pp_sm_list)){
    
    Sm.full  <-  pp_sm_list[[i]]$sm[[1]]
    
    smoothed_lines[[i]] <- Sm.full
  }
  
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- apply(smoothed_matrix, 1, median, na.rm = TRUE) #rowMeans(smoothed_matrix, na.rm = TRUE)

  return(list(smoothed_mean=smoothed_mean, model_name=model_name, sc_name=sc_name))
}

group_mean_cal_plots <- function(data2, dataset_name){

  filename <- paste0("comb_results/grouped_cal/paper_mean_cal_plot_", data2[[1]]$sc_name, "_", dataset_name, ".pdf")

  data <- list()
  count<-1
  for(i in seq_along(data2)){
    if(!grepl("corrW", data2[[i]]$model_name, ignore.case = TRUE)){
	if(grepl("propensity_model_missW_recalibrate", data2[[i]]$model_name, ignore.case = TRUE)){
		data2[[i]]$model_name = "Membership-based recalibration"
	}
	if(grepl("propensity_model_missW_addY", data2[[i]]$model_name, ignore.case = TRUE)){
		data2[[i]]$model_name = "Membership-based add-Y"
	}
	if(grepl("mah_model_missW_recalibrate", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Distance-based recalibration model"
	}
	if(grepl("mah_model_missW_addY", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Distance-based add-Y"
	}
	if(grepl("target_only", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Target-only"
	}
	if(grepl("intercept_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Intercept Recalibration-source only"
	}
	if(grepl("intercept_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Intercept Recalibration-all data"
	}
	if(grepl("logistic_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Logistic Recalibration-all data"
	}
	if(grepl("logistic_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Logistic Recalibration-source only"
	}
	if(grepl("bayesian_updating", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Bayesian"
	}
	data[[count]] <- data2[[i]]
	count <- count+1
    }
    
  }
  #data <- data %>% filter(!grepl("corrW|addY", model_name, ignore.case = TRUE))
  pdf(file = filename)
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed")
  abline(0,1)
  pp.full<- seq(0.001, 0.99, length=100)
  legend_colors <- c("#465177", "#BBB55A", "#E2C530", "#D79B1C", "#A96023",
                                                     "#6D402D", "#393F37", "#2A5638")
  legend_shapes <- seq(1,8) # 1 for circle, 2 for triangle
  print(data)
  for(i in seq_along(data)){
    lines(pp.full, data[[i]]$smoothed_mean, col = legend_colors[i], lty=legend_shapes[i], lwd=2)
  }
  legend("topleft", legend = sapply(data, function(x) x$model_name), col = legend_colors, lwd = 2, lty = legend_shapes, cex = 0.7)
  
  dev.off()
}



group_mean_cal_plots_ggplot <- function(data2, dataset_name) {
  # Define the output filename
    filename <- paste0("comb_results/grouped_cal/6Models_mean_cal_plot_", data2[[1]]$sc_name, "_", dataset_name, ".pdf")
  
  # Process and clean data for plotting
  data <- list()
  count <- 1
  for(i in seq_along(data2)){
    if(!grepl("corrW|mah|bayesian|addY", data2[[i]]$model_name, ignore.case = TRUE)){
	if(grepl("propensity_model_missW_recalibrate", data2[[i]]$model_name, ignore.case = TRUE)){
		data2[[i]]$model_name = "Membership-based recalibration"
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
		data2[[i]]$model_name = "Target-only"
	}
	if(grepl("intercept_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Intercept Recalibration-ancillary only"
	}
	if(grepl("intercept_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Intercept Recalibration-all data"
	}
	if(grepl("logistic_calibration_full", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Logistic Recalibration-all data"
	}
	if(grepl("logistic_calibration_source_only", data2[[i]]$model_name, ignore.case = TRUE) ){
		data2[[i]]$model_name = "Logistic Recalibration-ancillary only"
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
  # Define the plot
  ggplot(plot_data, aes(x = Predicted, y = Observed, color = ModelName, linetype=ModelName)) +
    geom_line(linewidth = 1) +scale_linetype_manual(values=seq(1,6))+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    theme_minimal() +
    labs(x = "Predicted", y = "Observed", color = "Model", linetype="Model") +
    theme(legend.position = "bottom", legend.text = element_text(size = 7), legend.title=element_text(size=9, angle = 90), legend.key.size = unit(0.5, "cm"))+guides(linetype=guide_legend(ncol=3), color=guide_legend(ncol=3))
  
  # Save the plot to a PDF
  ggsave(filename,  plot = last_plot(), width = 6, height = 5)
}



calibration_instability_plot_V3 <- function(pp_sm_list, sc_name) {
  #start.time <- Sys.time()
  
  smoothed_lines <- list()
  model_name <- pp_sm_list[[1]]['Model']
  filename <- paste0("comb_results/cal/cal_plot_", model_name,'_', sc_name, ".pdf")
  
  pdf(file = filename)
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed")
  
  abline(a = 0, b = 1, col = 'black')  # Add ideal line
  
  pp.full <- seq(0.001, 0.99, length = 100)
  for (i in 1:length(pp_sm_list)) {
    Sm.full <- pp_sm_list[[i]]$sm[[1]]
    lines(pp.full, Sm.full, col = alpha('grey', 0.3))  # Add lines for single iteration
    rug(Sm.full, side = 2, col = alpha('dark green', 0.4))  # Add rug plot on y-axis
    smoothed_lines[[i]] <- Sm.full
  }
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- apply(smoothed_matrix, 1, median, na.rm = TRUE)
  smoothed_quantiles <- apply(smoothed_matrix, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  lines(pp.full, smoothed_mean, col = "dark orange", lwd = 1)  # Add median line
  lines(pp.full, smoothed_quantiles[1, ], col = alpha("blue", 0.8), lwd = 1, lty = "dashed")  # Add lower band
  lines(pp.full, smoothed_quantiles[2, ], col = alpha("dark blue", 0.8), lwd = 1, lty = "dashed")  # Add upper band
  
  #polygon(c(pp.full, rev(pp.full)), c(smoothed_quantiles[1, ], rev(smoothed_quantiles[2, ])),
  #        col = alpha("dark orange", 0.2), border = NA)  # Add ribbon
  
  legend("bottomright", legend = c("Ideal", "Single iteration", "Median", "Lower band", "Upper band"),
         col = c('black', alpha('grey', 0.3), "dark orange", alpha("blue", 0.8), alpha("dark blue", 0.8)),
         lty = c("solid", "solid", "solid", "dashed", "dashed"),  bty = "n")
  
  dev.off()
  #end.time <- Sys.time()
  #time.taken <- round(end.time - start.time,2)
  #print(time.taken)
}



indv_stability_plot <- function(preds, sc_name){
  index=c("X91824", "X34403", "X62800", "X41490", "X74248", "X25010", "X85842", "X71263", "X79908")
  
  indv_stability_df <- data.frame( index = c(), ID = c(), iter = c(), risk = c())
  
  for (df in 1:length(preds)) {
    for(i in 1:length(index)){
      indv_stability_df <- rbind(indv_stability_df, list(index=index[i], ID=i, 
                                                         iter=df, risk=preds[[df]][index[i]][[1]]) ) 
    }
  }
  model_name <- preds[[1]]['X']
  filename <- paste0("comb_results/indv_stab/indv_stability_plot_", model_name,'_', sc_name, ".pdf")
  pdf(file = filename)
  set.seed(12344)
  plot <- ggplot(indv_stability_df, aes(x = factor(ID), y = risk, colour = factor(ID)) 
  ) +scale_color_wa_d("rainier"
  )+ylim(0,1)+ geom_jitter(alpha = 1,  size = 1, width=0.2
  ) +labs(x='Individuals', y='Predicted Risk'
  )+  theme_bw() + theme(legend.position = "none") 
  print(plot)
  dev.off()
}



