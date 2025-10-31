#instability in mean estimated risk
MER_plot <- function(MER_list, model, sc_name){
  model_name <- model
  filename <- paste0("mean_est_risk_", model_name,'_', sc_name, ".pdf")
  
  pdf(file = filename)
  plot(density(unlist(MER_list), na.rm =TRUE), xlab="Mean estimated risk", main="Instability in mean estimated risk")
  dev.off()
}


#instability in calibration curves + CI bands
calibration_instability_plot <- function(pp_sm_list, sc_name){
  smoothed_lines <- list()
  smoothed_lines_df <- data.frame(x = numeric(0), y = numeric(0), group = character(0))
  
  model_name <- pp_sm_list[[1]]['Model']
  filename <- paste0("cal_plot_", model_name,'_', sc_name, ".pdf")
  
  #pdf(file = filename)
  mainplot <- ggplot(data = NULL, aes(x = NULL, y = NULL)) +
    geom_blank() +  # To create an empty plot with specified limits and labels
    xlim(0, 1) +
    ylim(0, 1) +
    labs(x = "Predicted", y = "Observed") +
    geom_abline(aes(intercept = 0, slope = 1, color='Ideal'), key_glyph = "path")+
    theme_classic()
  
  
  pp.full<- seq(0.001, 0.99, length=100)
  for(i in 1:length(pp_sm_list)){
    Sm.full  <-  pp_sm_list[[i]]$sm[[1]]
    
    smoothed_lines[[i]] <- Sm.full
    df <- data.frame(x = pp.full, y = Sm.full)
    
    mainplot <- mainplot + 
      geom_path(data = df, aes(x = x, y = y, color = "Single iteration"))+
      geom_rug(data=df, aes( y = y), col = alpha('dark green', 0.06))
  }
  
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- apply(smoothed_matrix, 1, median, na.rm = TRUE) #rowMeans(smoothed_matrix, na.rm = TRUE)
  smoothed_quantiles <- apply(smoothed_matrix, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  mainplot <- mainplot+
    geom_ribbon(aes(x = pp.full, ymin = smoothed_quantiles[1, ], ymax = smoothed_quantiles[2, ]), alpha = 0.2, fill = 'dark orange') +
    geom_line(aes(x = pp.full, y = smoothed_mean, color = "Median"), linewidth = 0.5) +
    geom_line(aes(x = pp.full, y = smoothed_quantiles[1, ], color = "Lower band"), linewidth = 0.5,linetype = "dashed") +
    geom_line(aes(x = pp.full, y =  smoothed_quantiles[2, ], color = "Upper band"), linewidth = 0.5, linetype = "dashed")+  
    scale_color_manual(name = "Calibration curve", 
                       values = c('Ideal'='black', 
                                  'Single iteration'=alpha('grey', 0.3), 
                                  "Median" = "blue",  # Transparent blue
                                  "Lower band" = alpha("dark blue", 0.8),  # Transparent dark blue
                                  "Upper band" = alpha("dark blue", 0.8)
                       ),
                       guide = guide_legend(override.aes = list(linetype = c("solid",'dashed', "solid", "solid", 'dashed')))
    ) +labs(x = "Predicted", y = "Observed")  # Axis labels
  
  
  
  ggsave(file = filename, plot=mainplot, device = 'pdf')
  #dev.off()
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
  filename <- paste0("indv_stability_plot_", model_name,'_', sc_name, ".pdf")
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




get_val_data <- function(df){
  models <- unique(df$Model)
  temp <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Model", "AUC", "CITL", "CSLOPE", "BrierScore"))
  
  for(i in 1:length(models)){
    model_df <- subset(df, grepl(models[i], Model))
    x <- data.frame(Model=models[i], AUC= get_ci(model_df$AUC), CITL=get_ci(model_df$CITL),
                    CSLOPE=get_ci(model_df$CSLOPE), BrierScore=get_ci(model_df$BrierScore)
    )
    temp <- rbind(temp, x)
    
  }
  return(temp)
}

get_ci <- function(data){
  data <- sort(data)
  return(list(median = median(data), lower_band= data[5], upper_band = data[195]))#5, 195
}



val_metric_plot<- function(df, scenario_name, dataset_name){ 
  labels <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore')
  legends <- list()
  figs <- list()
  plots <- 0
  for(i in 1:4){
    data <- df %>% dplyr::select(matches(paste0("Model|", labels[i])))
    #print(data)
    colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
    
    figs[[i]] <- ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(ymin = lower_band , ymax = upper_band), width = 0
      )  +  scale_shape_manual(values=seq(1,14))+ geom_point(size=2) +
      ylim(min(pmin(data$lower_band,data$Median)), max(pmax(data$upper_band,data$Median)))  +
      theme(axis.title.x =element_blank(), axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.title.y=element_text(size=9),
            legend.position='none'  )+ labs(y= labels[i], colour="Model", shape="Model") +guides(colour = guide_legend(nrow =3))
    
    plots<-  ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +       
      scale_shape_manual(values=seq(1,14))+ geom_point(size=2) +
      theme( legend.text=element_text(size=7), legend.title=element_text(size=8), legend.key.size = unit(0.5, "cm")
      ) + labs(y= labels[i], colour="Model", shape="Model")#+ guides(colour = guide_legend(override.aes = list(size = 0.5)))
    
  }
  
  combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 2, nrow = 2)
  
  grobs <- ggplotGrob(plots)$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
  
  
  #print(legend)
  
  combined_plot <- plot_grid(combined_plot, NULL, legend, NULL,  nrow=1,rel_widths = c(1, -0.2, 1, -0.2))  
  
  
  filename <- paste0("validation_mterics_", scenario_name, "_", dataset_name, ".pdf")
  
  ggsave(filename, plot = combined_plot, width =7, height = 3.1)
  print('Done')
  print(filename)
  
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

group_mean_cal_plots <- function(data, dataset_name){
  filename <- paste0("mean_cal_plot_", data[[1]]$sc_name, "_", dataset_name, ".pdf")
  
  pdf(file = filename)
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed")
  abline(0,1)
  pp.full<- seq(0.001, 0.99, length=100)
  legend_colors <- c("#465177", "#BBB55A", "#E2C530", "#D79B1C", "#A96023",
                     "#6D402D", "#393F37", "#2A5638", "#538034", "#82A656",
                     "#99B8A1", "#A0B3E1", "#BD8FE2", "#DF3383")
  legend_shapes <- seq(1,14) # 1 for circle, 2 for triangle
  
  for(i in seq_along(data)){
    lines(pp.full, data[[i]]$smoothed_mean, col = legend_colors[i], lty=legend_shapes[i], lwd=2)
  }
  legend("topleft", legend = sapply(data, function(x) x$model_name), col = legend_colors, lwd = 2, lty = legend_shapes, cex = 0.7)
  
  dev.off()
}



calibration_instability_plot_V3 <- function(pp_sm_list, sc_name) {
  #start.time <- Sys.time()
  
  smoothed_lines <- list()
  model_name <- pp_sm_list[[1]]['Model']
  filename <- paste0("cal_plot_", model_name,'_', sc_name, ".pdf")
  
  pdf(file = filename)
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed")
  
  abline(a = 0, b = 1, col = 'black')  # Add ideal line
  
  pp.full <- seq(0.001, 0.99, length = 100)
  for (i in 1:length(pp_sm_list)) {
    Sm.full <- pp_sm_list[[i]]$sm[[1]]
    lines(pp.full, Sm.full, col = alpha('grey', 0.3))  # Add lines for single iteration
    rug(Sm.full, side = 2, col = alpha('dark green', 0.6))  # Add rug plot on y-axis
    smoothed_lines[[i]] <- Sm.full
  }
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- apply(smoothed_matrix, 1, median, na.rm = TRUE)
  smoothed_quantiles <- apply(smoothed_matrix, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  lines(pp.full, smoothed_mean, col = "blue", lwd = 1)  # Add median line
  lines(pp.full, smoothed_quantiles[1, ], col = alpha("dark blue", 0.8), lwd = 1, lty = "dashed")  # Add lower band
  lines(pp.full, smoothed_quantiles[2, ], col = alpha("dark blue", 0.8), lwd = 1, lty = "dashed")  # Add upper band
  
  polygon(c(pp.full, rev(pp.full)), c(smoothed_quantiles[1, ], rev(smoothed_quantiles[2, ])),
          col = alpha("dark orange", 0.2), border = NA)  # Add ribbon
  
  legend("bottomright", legend = c("Ideal", "Single iteration", "Median", "Lower band", "Upper band"),
         col = c('black', alpha('grey', 0.3), "blue", alpha("dark blue", 0.8), alpha("dark blue", 0.8)),
         lty = c("solid", "solid", "solid", "dashed", "dashed"),  bty = "n")
  
  dev.off()
  #end.time <- Sys.time()
  #time.taken <- round(end.time - start.time,2)
  #print(time.taken)
}


