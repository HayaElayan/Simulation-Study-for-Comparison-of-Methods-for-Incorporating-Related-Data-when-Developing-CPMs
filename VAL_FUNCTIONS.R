#bootstrap validation results
proposed_model_validation_results <- function(model, validation){
  if(all(is.na(model))){
    val_results <- matrix(nrow = 1,ncol = 5)
    colnames(val_results) <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore', 'MER') #'MER' = mean estimated risk
    
    val_results[1,1] <- NA
    val_results[1,2] <- NA
    val_results[1,3] <- NA
    val_results[1,4] <- NA
    val_results[1,5] <- NA
    
    Sm.full <- rep(NA, 100)
    preds <- rep(NA, 9)
    return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
  }else{
    val_results <- matrix(nrow = 1,ncol = 5)
    colnames(val_results) <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore', 'MER') #'MER' = mean estimated risk
    
    
    pr_val <- predict(model, type="response", newdata = validation) # predict probabilities 
    lp_val <- predict(model, newdata = validation ) # predict lp type=link
    
    # print(pr_val[1:10])
    # print(lp_val[1:10])
    # calculate performance of the  model in the validation sample
    val_cstat_model <- roc(Y ~ pr_val,data=validation)
    val_results[1,1] <- val_cstat_model$auc
    
    val_results[1,4] <- BrierScore(validation$Y, pred=pr_val)#BrierScore(model)
    val_results[1,5] <- mean(pr_val)
    index=c(91824, 34403, 62800, 41490, 74248, 25010, 85842, 71263, 79908)
    preds <- pr_val[index]
    tryCatch(
      { 
        val_citl_model <- glm(Y ~ offset(lp_val),family=binomial, data=validation)
        val_results[1,2] <- summary(val_citl_model)$coefficients[1,1]
        
        val_cslope_model <- glm(Y ~ lp_val,family=binomial(link='logit'), data=validation)
        val_results[1,3] <- summary(val_cslope_model)$coefficients[2,1]
        
        
        Sm       <- lowess(pr_val, validation$Y, iter = 0)
        #pp.full  <- seq(min(pr_val), max(pr_val), length = 100) #xxxx
        pp.full<- seq(0.001, 0.99, length=100)
        Sm.full  <- approx(Sm, xout = pp.full, ties = mean)$y #yyyyy
        
        
        return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
      }, error=function(e){
        val_results[1,2] <- NA
        val_results[1,3] <- NA
        
        Sm.full <- rep(NA, 100)
        
        return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
      }
      
    )
  }
}



calibrated_model_validation_results <- function(model, type, validation){
  
  val_results <- matrix(nrow = 1,ncol = 5)
  colnames(val_results) <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore', 'MER') #'MER' = mean estimated risk
  pr_val <- nrow(validation)
  lp_val <- nrow(validation)
  
  validation$lp <- predict(model$source_model, newdata = validation, type = "link")
  #print(table(validation$Y, validation$lp))
  
  if(type=='intercept_only'){
    pr_val <- predict(model$calibrated_model, type="response", newdata = validation, offset = validation$lp) # predict probabilities 
    lp_val <- predict(model$calibrated_model, newdata = validation, offset = validation$lp ) # predict lp type=link
    #print(lp_val)
  }else{
    pr_val <- predict(model$calibrated_model, type="response", newdata = validation, lp = validation$lp) # predict probabilities 
    lp_val <- predict(model$calibrated_model, newdata = validation, lp = validation$lp ) # predict lp type=link
  }
  
  
  # calculate performance of the  model in the validation sample
  val_cstat_model <- roc(Y ~ pr_val,data=validation)
  val_results[1,1] <- val_cstat_model$auc
  
  val_citl_model <- glm(Y ~ offset(lp_val),family=binomial, data=validation)
  
  
  val_cslope_model <- glm(Y ~ lp_val,family=binomial(link='logit'), data=validation)
  
  
  val_results[1,4] <- BrierScore(validation$Y, pred=pr_val) #BrierScore(model$calibrated_model)

  val_results[1,5] <- mean(pr_val)
  
  tryCatch(
    {
      val_results[1,2] <- summary(val_citl_model)$coefficients[1,1]
      val_results[1,3] <- summary(val_cslope_model)$coefficients[2,1]
      
      Sm       <- lowess(pr_val, validation$Y, iter = 0)
      pp.full<- seq(0.001, 0.99, length=100)
      Sm.full  <- approx(Sm, xout = pp.full, ties = mean)$y #yyyyy
      index=c(91824, 34403, 62800, 41490, 74248, 25010, 85842, 71263, 79908)
      preds <- pr_val[index]
      return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
      
    }, error=function(e){
      val_results[1,2] <- NA
      val_results[1,3] <- NA
      Sm.full <- rep(NA, 100)
      
      index=c(91824, 34403, 62800, 41490, 74248, 25010, 85842, 71263, 79908)
      preds <- pr_val[index]
      return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
    }
  )
}



dynamic_model_validation_results <- function(val_data, theta_hat) {
  val_results <- matrix(nrow = 1,ncol = 5)
  colnames(val_results) <- c('AUC', 'CITL', 'CSLOPE', 'BrierScore', 'MER') #'MER' = mean estimated risk
  
  val_data_temp <- val_data%>% dplyr::select(-c( isSource, Y, U))
  # Add intercept term to new_data
  Xt<-as.matrix(sapply(val_data_temp, as.numeric))
  Xt <- cbind(1, Xt) # Add intercept
  
  lp <- as.vector(Xt %*% theta_hat)
  # Compute predicted probabilities
  pred_prob <- 1 / (1 + exp(-lp))
  
  
  # print(lp[1:10])
  # print(pred_prob[1:10])
  
  val_cstat_model <- roc(Y ~ as.double(pred_prob),data=val_data)
  val_results[1,1] <- val_cstat_model$auc
  
  val_citl_model <- glm(Y ~ offset(lp),family=binomial, data=val_data)
  val_results[1,2] <- summary(val_citl_model)$coefficients[1,1]
  
  val_cslope_model <- glm(Y ~ lp,family=binomial(link='logit'), data=val_data)
  if(is.na(val_cslope_model$coef[2])){
    val_results[1,3] <- NA
  }else{
    val_results[1,3] <- summary(val_cslope_model)$coefficients[2,1]
  }
  
  brier_score <- BrierScore(val_data$Y , pred= pred_prob)#mean((val_data$Y - pred_prob)^2)
  val_results[1,4] <- brier_score
  val_results[1,5] <- mean(pred_prob)
  
  Sm       <- lowess(pred_prob, val_data$Y, iter = 0)
  pp.full<- seq(0.001, 0.99, length=100)
  index=c(91824, 34403, 62800, 41490, 74248, 25010, 85842, 71263, 79908)
  preds <- pred_prob[index]
  
  tryCatch( {
    Sm.full  <- approx(Sm, xout = pp.full, ties = mean)$y 
    return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
  }, error=function(e){ 
    Sm.full <- rep(NA, 100)
    return(list(val_results = as.data.frame(val_results), cal_plot = list(sm = Sm.full), preds = preds) )
  })
}




#instability in mean estimated risk
MER_plot <- function(MER_list, model, sc_name){
  model_name <- model
  filename <- paste0("mean_est_risk_", model_name,'_', sc_name, ".pdf")
  
  pdf(file = filename)
  plot(density(unlist(MER_list)), xlab="Mean estimated risk", main="Instability in mean estimated risk")
  dev.off()
}


#instability in calibration curves + CI bands
calibration_instability_plot <- function(pp_sm_list, sc_name){
  smoothed_lines <- list()
  
  model_name <- pp_sm_list[[1]]['Model']
  filename <- paste0("cal_plot_", model_name,'_', sc_name, ".pdf")
  
  pdf(file = filename)
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted", ylab = "Observed")
  abline(0,1)
  
  pp.full<- seq(0.001, 0.99, length=100)
  for(i in 1:length(pp_sm_list)){
    
    Sm.full  <-  pp_sm_list[[i]]$sm[[1]]
    
    smoothed_lines[[i]] <- Sm.full
    
    lines( pp.full,  Sm.full, col='grey',xlim=c(0, 1) , ylim=c(0, 1) )
  }
  
  
  smoothed_matrix <- do.call(cbind, smoothed_lines)
  smoothed_mean <- rowMeans(smoothed_matrix, na.rm = TRUE)
  smoothed_quantiles <- apply(smoothed_matrix, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Plot mean and confidence bands
  lines(pp.full, smoothed_mean, col = 'blue', lwd = 2)
  lines(pp.full, smoothed_quantiles[2, ], col = 'red', lty = 2)
  lines(pp.full, smoothed_quantiles[1, ], col = 'red', lty = 2)
  dev.off()
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
  for(i in 1:4){
    data <- df %>%
      select(matches(paste0("Model|", labels[i])))
    #print(data)
    colnames(data) <- c('Model', 'Median', 'lower_band', 'upper_band')
    
    figs[[i]] <- ggplot(data, aes(x=Model, y=Median, colour=Model, shape=Model)) +        # ggplot2 plot with confidence intervals
      geom_errorbar(aes(ymin = lower_band , ymax = upper_band), width = 0
      )  +  scale_shape_manual(values=seq(1,14))+ geom_point(size=2) +
      ylim(min(pmin(data$lower_band,data$Median)), max(pmax(data$upper_band,data$Median)))  +
      theme(axis.title.x =element_blank(), axis.ticks.x = element_blank(), axis.text.x =element_blank(),axis.title.y=element_text(size=11),
            legend.position='none' , legend.text=element_text(size=7), legend.title = element_text(angle=90, hjust = 0.5, vjust = 0.5)
      )+ labs(y= labels[i], colour="Model", shape="Model") +guides(colour = guide_legend(nrow =3))
    
  }
  
  combined_plot <- grid.arrange(figs[[1]],figs[[2]], figs[[3]],  figs[[4]], ncol = 4, nrow = 1)
  
  legend <- get_legend(figs[[1]] + theme(legend.position = "bottom"))  # Get legend from any plot
  combined_plot <- arrangeGrob(combined_plot, legend, heights = unit(c(2.1, 1), "null"))
  
  filename <- paste0("validation_mterics_", scenario_name, "_", dataset_name, ".pdf")
  
  ggsave(filename, plot = combined_plot, width = 11, height = 3)
  
}



