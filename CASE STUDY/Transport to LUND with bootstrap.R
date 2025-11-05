source("FUNCTIONS.R")
source("LAMBDA.R")
library(MLmetrics)
library(pROC)
library(DescTools)
library(dplyr)


#Read data


transport_centre <- read.csv("imputed_data_LUND.csv")
original_centres <- read.csv("imputed_data_all_centres_except_LUND.csv")

source <- original_centres
target <- transport_centre

nrow(original_centres)
nrow(transport_centre)

dev_formula <- as.formula( "outcome~Age+Sex+AF_atrial_flutter+Diabetes+BMI+LVEF35+eGFR")

model_original <- glm(dev_formula, data=original_centres, family=binomial)
original_transport <- validation_results(model_original, transport_centre)

#Correcting for shift

#1- get propensity score weights with limiting weights up to 1, 2- create a weighted model to correct for shift 3- validate
ps_weights_limit1 <- propensity_weighting_limit1(source, target)
propensity_model_limit1 <- weighted_LR(source, target, ps_weights_limit1)
internal_prop_Lim_val <- ps_validation_results(propensity_model_limit1, target)


#1- get propensity score weights with limiting weights up to 1, 2- create a weighted model to correct for shift with forgetting factor 3- validate
ps_weights_lambda <- propensity_weighting_with_lambda(source, target)
propensity_model_lambda <- weighted_LR(source, target, ps_weights_lambda)
internal_prop_lambda_val <- ps_validation_results(propensity_model_lambda, target)



#intercept recalibration, develop the model on full data then recalibrate of target
int_calib_full_df <- intercept_calibration(source, target, "FULL", dev_formula)
internal_int_full_val <- calibrated_model_validation_results(int_calib_full_df, target, "intercept_only")

#intercept recalibration, develop the model on source data then recalibrate of target
int_calib_source_only <- intercept_calibration(source, target, "source_only", dev_formula)
internal_int_source_val <- calibrated_model_validation_results(int_calib_source_only, target, "intercept_only")


#logistic recalibration, develop the model on full data then recalibrate of target
logit_calib_full_df <- logistic_calibration(source, target, "FULL", dev_formula)
internal_logistic_full_val <- calibrated_model_validation_results(logit_calib_full_df, target, "logistic")

#logistic recalibration, develop the model on source data then recalibrate of target
logit_calib_source_only <- logistic_calibration(source, target, "source_only", dev_formula)
internal_logistic_source_val <- calibrated_model_validation_results(logit_calib_source_only, target, "logistic")


model_on_allData <- all_data_model(source, target)  
internal_model_on_allData_val <- ps_validation_results(model_on_allData, target)


model_on_targetOnly <- glm(dev_formula, data=target, family=binomial)
internal_model_on_targetOnly_val <- validation_results(model_on_targetOnly, target)


val_results_df <- rbind(
  original_transport$val_results,
  internal_prop_Lim_val$val_results,
  internal_prop_lambda_val$val_results,
  internal_int_full_val$val_results,
  internal_int_source_val$val_results,
  internal_logistic_full_val$val_results,
  internal_logistic_source_val$val_results,
  internal_model_on_allData_val$val_results,
  internal_model_on_targetOnly_val$val_results
)

val_results_df$model_name <- c("Naive Logistic-developed on source only and validated on LUND", 
                               "Membership-based weighted model (weights limited to 1)",
                               "Membership-based weighted model (weights limited to 1 + Forgetting factor)",
                               "Intercept recalibration-developed on full data",
                               "Intercept recalibration-developed on source only", 
                               "Logistic recalibration-developed on full data",
                               "Logistic recalibration-developed on source only",
                               "Naive Logistic-developed on full data",
                               "Naive Logistic-developed on target only"
)

write.csv(val_results_df, "internal validation-developed on all except LUND.csv")


boot_results<- manual_boot(source, target, 200, dev_formula)

write.csv(boot_results$apparent, paste0("bootstrap_apparent_validation_results-developed on all except LUND.csv"))
write.csv(boot_results$test, "bootstrap_test_validation_results-developed on all except LUND.csv")

