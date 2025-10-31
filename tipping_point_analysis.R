all_dfs <- data.frame()

for(DS in c(1,2,4)){
  for(it in 1:200){
    
    df <- read.csv(paste0("../raw_results/validation/validation_results_ID1_IT",it,"_TS0.25_DS",DS,".csv"))
    df <- subset(df,select = -c(MER))
    
    
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
    df$DS <- DS
    all_dfs <- rbind(all_dfs, df)
  
  }
}

write_csv(all_dfs, "../raw_results/all_iters_id1.csv")
########################################



all_dfs<- read.csv("../raw_results/all_iters_id1.csv")

df_long <- all_dfs %>%
  pivot_longer(
    cols = c(AUC, CITL, CSLOPE, BrierScore),
    names_to = "metric",
    values_to = "value"
  )

df_delta <- df_long %>%
  group_by(iter, DS, metric) %>%
  mutate(
    target_value = value[Model == "Target-only"][1],  # ensure single value
    delta = value - target_value
  ) %>%
  ungroup() %>%
  filter(Model != "Target-only") %>%
  select(-target_value)


summary_delta <- df_delta %>%
  group_by(Model, DS, metric) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    low = quantile(delta, 0.025, na.rm = TRUE),
    high = quantile(delta, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


summary_delta$DS <- factor(summary_delta$DS,
                           levels = c(1, 2, 4),
                           labels = c(
                             bquote(bold( "(" ~ "N"["min"] ~ ")")),
                             bquote(bold("(2 ×" ~ "N"["min"] ~ ")")),
                             bquote(bold("(4 ×" ~ "N"["min"] ~ ")"))
                           ))




# Assign a different shape per model
model_shapes <- c("Membership-based recalibration" = 16,
                  "Intercept Recalibration-all data" = 17,
                  "Intercept Recalibration-ancillary only" = 15,
                  "Logistic Recalibration-all data" = 18,
                  "Logistic Recalibration-ancillary only" = 19)

# Plot

pdf("tipping_point_analysis_Noshift.pdf", width =8, height = 5)
ggplot(summary_delta, aes(x = DS, y = mean_delta, color = Model, group = Model)) +
  geom_line(size = 1) +
  geom_point(aes(shape = Model), size = 3) +
  scale_shape_manual(values = model_shapes) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete(labels = function(x) parse(text = x)) +  # <- parse labels
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    x = "Development Sample Size",
    y = expression("Mean"~ Delta ~ "(Model - Target-only Model)"),
    color = "Model",
    shape = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 10),  # smaller x-axis text
    axis.title.y = element_text(size = 10)) +
  guides(
    color = guide_legend(ncol = 2, byrow = TRUE),
    shape = guide_legend(ncol = 2, byrow = TRUE)
  )


dev.off()
