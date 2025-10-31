set.seed(123)
all_dfs <- data.frame()

# Example AUC results from 200 simulation iterations
for(it in 1:200){
  
  df <- read.csv(paste0("../raw_results/validation/validation_results_ID5_IT",it,"_TS0.25_DS",4,".csv"))
  df <- subset(df,select = -c(MER, CSLOPE, CITL, BrierScore))
  
  
  df <- df %>% dplyr::filter(!grepl("corrW|noLambda|mah|addY|bayesian|model4|model5", Model, ignore.case = TRUE))
  
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
  df$iter <- it
  all_dfs <- rbind(all_dfs, df)
  
}
all_dfs

df_wide <- all_dfs |>
  pivot_wider(names_from = Model, values_from = AUC)

head(df_wide)


# Identify baseline model
baseline <- "Membership-based recalibration"

# List of other models
other_models <- setdiff(names(df_wide), c("iter", baseline))

# Perform paired Wilcoxon tests
results <- lapply(other_models, function(model) {
  test <- wilcox.test(df_wide[[model]], df_wide[[baseline]], paired = TRUE)
  data.frame(
    Model = model,
    p_value = test$p.value,
    statistic = test$statistic
  )
})

results_df <- do.call(rbind, results)


medians <- all_dfs %>%
  group_by(Model) %>%
  summarise(median_AUC = median(AUC))

results_summary <- left_join(results_df, medians, by = "Model")

# Compute ΔAUC vs baseline
baseline_median <- medians$median_AUC[medians$Model == "Membership-based recalibration"]
results_summary$delta_AUC <- results_summary$median_AUC - baseline_median

print(results_summary)


