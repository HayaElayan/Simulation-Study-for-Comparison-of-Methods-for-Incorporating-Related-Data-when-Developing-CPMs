#Rscript task_file_name.R file1 file2 file3 $SGE_TASK_ID > output_file_name.$SGE_TASK_ID
#args[number] based on the argument order when passing the job
start.time <- Sys.time()

args <- commandArgs(trailingOnly = T) #pull in all arguments from the qsub file

# file1 <- args[1]
# file2 <- args[2]
# file3 <- args[3]
row <- as.numeric(args[1]) #Note that we need to specify what class the argument is

# Load required libraries
library(ROCR)
library(MLmetrics)
library(rms)
library(dplyr)
library(pmsampsize)
library(MASS)
library(pROC)
library(Metrics) 
library(matrixcalc)
library(DescTools)
library(predtools)
library(tidyr)
library(dma)
library(magrittr)
library(ggplot2)
#library(ggpubr)
library(wacolors)

source("SOURCE_PARAMETERS.R")
source("TARGET_PARAMETERS.R")
source("DEV_FUNCTIONS.R")
source("VAL_FUNCTIONS.R")
source("READ_VALIDATION.R")


variances_delta<- get_variances_delta(SOURCE.PARAM[row,])
means_delta <- get_means_delta(SOURCE.PARAM[row,])
binary_prev_delta <- get_binary_prev_delta(SOURCE.PARAM[row,])
B_coeffcients_delta<-  get_B_coeffcients_delta(SOURCE.PARAM[row,])
dev_samp_size <- get_dev_samp_size(SOURCE.PARAM[row,])
target_split <- get_target_split(SOURCE.PARAM[row,])
iteration <- get_iteration(SOURCE.PARAM[row,])
scenario_ID <- get_ID(SOURCE.PARAM[row,])

#DATASETS
source_target_model15 <- generate_development( N_SAMPLES, target_split, binary_predictors,
                                               means_target, means_delta, CORR_MATRIX, variances_delta,
                                               binary_prev_target, binary_prev_delta, predictors_names,
                                               B_coeffcients_target_model15,  B_coeffcients_delta, iteration)


source_target_model5 <- generate_development( N_SAMPLES, target_split, binary_predictors,
                                              means_target, means_delta, CORR_MATRIX, variances_delta,
                                              binary_prev_target, binary_prev_delta, predictors_names,
                                              B_coeffcients_target_model5,  B_coeffcients_delta, iteration)

source_target_model4 <- generate_development( N_SAMPLES, target_split, binary_predictors,
                                              means_target, means_delta, CORR_MATRIX, variances_delta,
                                              binary_prev_target, binary_prev_delta, predictors_names,
                                              B_coeffcients_target_model4,  B_coeffcients_delta, iteration)


N_min_model15 <- sampsize_calc(source_target_model15, predictors_names)
N_min_model5 <- sampsize_calc(source_target_model5, predictors_names)
N_min_model4 <- sampsize_calc(source_target_model4, predictors_names)

rm(source_target_model15); rm(source_target_model5);  rm(source_target_model4);

source_target_model15 <- generate_development( N_min_model15 * dev_samp_size , target_split, binary_predictors,
                                               means_target, means_delta, CORR_MATRIX, variances_delta,
                                               binary_prev_target, binary_prev_delta, predictors_names,
                                               B_coeffcients_target_model15,  B_coeffcients_delta, iteration)

source_target_model5 <- generate_development( N_min_model5 * dev_samp_size , target_split, binary_predictors,
                                              means_target, means_delta, CORR_MATRIX, variances_delta,
                                              binary_prev_target, binary_prev_delta, predictors_names,
                                              B_coeffcients_target_model5,  B_coeffcients_delta, iteration)


source_target_model4 <- generate_development( N_min_model4 * dev_samp_size , target_split, binary_predictors,
                                              means_target, means_delta, CORR_MATRIX, variances_delta,
                                              binary_prev_target, binary_prev_delta, predictors_names,
                                              B_coeffcients_target_model4,  B_coeffcients_delta, iteration)


########dataset model 15 predictors##################

#WEIGHTS
prop_weight_corr_inY_model15 <- propensity_weighting_with_lambda(source_target_model15, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_corr_exY_model15 <- propensity_weighting_with_lambda(source_target_model15, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

prop_weight_miss_inY_model15 <- propensity_weighting_with_lambda(source_target_model15, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_miss_exY_model15 <- propensity_weighting_with_lambda(source_target_model15, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_corr_inY_model15 <-  mahalanobis_weighting_with_lambda(source_target_model15, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_corr_exY_model15 <-  mahalanobis_weighting_with_lambda(source_target_model15, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_miss_inY_model15 <-  mahalanobis_weighting_with_lambda(source_target_model15, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_miss_exY_model15 <-  mahalanobis_weighting_with_lambda(source_target_model15, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

# MODELS

#propensity models
propensity_model_corrW_addY_model15 <- weighted_LR(source_target_model15, prop_weight_corr_inY_model15,  included_Y_in_weights='include')
propensity_model_corrW_recalibrate_model15 <- weighted_LR(source_target_model15, prop_weight_corr_exY_model15,  included_Y_in_weights='exclude')

propensity_model_missW_addY_model15 <- weighted_LR(source_target_model15, prop_weight_miss_inY_model15,  included_Y_in_weights='include')
propensity_model_missW_recalibrate_model15 <- weighted_LR(source_target_model15, prop_weight_miss_exY_model15,  included_Y_in_weights='exclude')

#distance models
mah_model_corrW_addY_model15 <- weighted_LR(source_target_model15, mah_weight_corr_inY_model15,  included_Y_in_weights='include')
mah_model_corrW_recalibrate_model15 <- weighted_LR(source_target_model15, mah_weight_corr_exY_model15,  included_Y_in_weights='exclude')

mah_model_missW_addY_model15 <- weighted_LR(source_target_model15, mah_weight_miss_inY_model15,  included_Y_in_weights='include')
mah_model_missW_recalibrate_model15 <- weighted_LR(source_target_model15, mah_weight_miss_exY_model15,  included_Y_in_weights='exclude')



#updating methods models
logistic_calibration_full_model15<- logistic_calibration(source_target_model15, development_data='FULL')
logistic_calibration_source_only_model15 <- logistic_calibration(source_target_model15, development_data='source_only')


intercept_calibration_full_model15<- intercept_calibration(source_target_model15, development_data='FULL') #make predictions with predict(oo1, type='response')
intercept_calibration_source_only_model15 <- intercept_calibration(source_target_model15, development_data='source_only')

#target data only model
target_only_model_model15 <- target_only_LR(source_target_model15)

#dynamic logistic model
theta_updated_model15 <- dynamic_model_theta_with_lambda(source_target_model15,  lambda_hyperparam_values, folds_num, iteration)

##VALIDATION

#propensity models validation
propensity_model_corrW_addY_model15_results<-proposed_model_validation_results(propensity_model_corrW_addY_model15, validation_data_model15)

propensity_model_missW_addY_model15_results<-proposed_model_validation_results(propensity_model_missW_addY_model15, validation_data_model15)

propensity_model_corrW_recalibrate_model15_results<-proposed_model_validation_results(propensity_model_corrW_recalibrate_model15, validation_data_model15)

propensity_model_missW_recalibrate_model15_results<-proposed_model_validation_results(propensity_model_missW_recalibrate_model15, validation_data_model15)

#ditance models validation
mah_model_corrW_addY_model15_results<-proposed_model_validation_results(mah_model_corrW_addY_model15, validation_data_model15)

mah_model_missW_addY_model15_results<-proposed_model_validation_results(mah_model_missW_addY_model15, validation_data_model15)

mah_model_corrW_recalibrate_model15_results<-proposed_model_validation_results(mah_model_corrW_recalibrate_model15, validation_data_model15)

mah_model_missW_recalibrate_model15_results<-proposed_model_validation_results(mah_model_missW_recalibrate_model15, validation_data_model15)


#target only model validation
target_only_model_model15_results<-proposed_model_validation_results(target_only_model_model15, validation_data_model15)

#calibration models validation
intercept_calibration_full_model15_results<-calibrated_model_validation_results(intercept_calibration_full_model15, type='intercept_only', validation_data_model15 )

intercept_calibration_source_only_model15_results<-calibrated_model_validation_results(intercept_calibration_source_only_model15, type='intercept_only', validation_data_model15 )

logistic_calibration_full_model15_results<-calibrated_model_validation_results(logistic_calibration_full_model15, type='logistic', validation_data_model15 )

logistic_calibration_source_only_model15_results<-calibrated_model_validation_results(logistic_calibration_source_only_model15, type='logistic', validation_data_model15 )


#dynamic model validation
theta_updated_model15_results <- dynamic_model_validation_results(validation_data_model15 , theta_updated_model15)



########dataset model 5 predictors##################

#WEIGHTS
prop_weight_corr_inY_model5 <- propensity_weighting_with_lambda(source_target_model5, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_corr_exY_model5 <- propensity_weighting_with_lambda(source_target_model5, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

prop_weight_miss_inY_model5 <- propensity_weighting_with_lambda(source_target_model5, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_miss_exY_model5 <- propensity_weighting_with_lambda(source_target_model5, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_corr_inY_model5 <-  mahalanobis_weighting_with_lambda(source_target_model5, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_corr_exY_model5 <-  mahalanobis_weighting_with_lambda(source_target_model5, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_miss_inY_model5 <-  mahalanobis_weighting_with_lambda(source_target_model5, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_miss_exY_model5 <-  mahalanobis_weighting_with_lambda(source_target_model5, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

# MODELS

#propensity models
propensity_model_corrW_addY_model5 <- weighted_LR(source_target_model5, prop_weight_corr_inY_model5,  included_Y_in_weights='include')
propensity_model_corrW_recalibrate_model5 <- weighted_LR(source_target_model5, prop_weight_corr_exY_model5,  included_Y_in_weights='exclude')

propensity_model_missW_addY_model5 <- weighted_LR(source_target_model5, prop_weight_miss_inY_model5,  included_Y_in_weights='include')
propensity_model_missW_recalibrate_model5 <- weighted_LR(source_target_model5, prop_weight_miss_exY_model5,  included_Y_in_weights='exclude')

#distance models
mah_model_corrW_addY_model5 <- weighted_LR(source_target_model5, mah_weight_corr_inY_model5,  included_Y_in_weights='include')
mah_model_corrW_recalibrate_model5 <- weighted_LR(source_target_model5, mah_weight_corr_exY_model5,  included_Y_in_weights='exclude')

mah_model_missW_addY_model5 <- weighted_LR(source_target_model5, mah_weight_miss_inY_model5,  included_Y_in_weights='include')
mah_model_missW_recalibrate_model5 <- weighted_LR(source_target_model5, mah_weight_miss_exY_model5,  included_Y_in_weights='exclude')



#updating methods models
logistic_calibration_full_model5<- logistic_calibration(source_target_model5, development_data='FULL')
logistic_calibration_source_only_model5 <- logistic_calibration(source_target_model5, development_data='source_only')


intercept_calibration_full_model5<- intercept_calibration(source_target_model5, development_data='FULL') #make predictions with predict(oo1, type='response')
intercept_calibration_source_only_model5 <- intercept_calibration(source_target_model5, development_data='source_only')

#target data only model
target_only_model_model5 <- target_only_LR(source_target_model5)

#dynamic logistic model
theta_updated_model5 <- dynamic_model_theta_with_lambda(source_target_model5,  lambda_hyperparam_values, folds_num, iteration)

##VALIDATION

#propensity models validation
propensity_model_corrW_addY_model5_results<-proposed_model_validation_results(propensity_model_corrW_addY_model5, validation_data_model5)

propensity_model_missW_addY_model5_results<-proposed_model_validation_results(propensity_model_missW_addY_model5, validation_data_model5)

propensity_model_corrW_recalibrate_model5_results<-proposed_model_validation_results(propensity_model_corrW_recalibrate_model5, validation_data_model5)

propensity_model_missW_recalibrate_model5_results<-proposed_model_validation_results(propensity_model_missW_recalibrate_model5, validation_data_model5)

#ditance models validation
mah_model_corrW_addY_model5_results<-proposed_model_validation_results(mah_model_corrW_addY_model5, validation_data_model5)

mah_model_missW_addY_model5_results<-proposed_model_validation_results(mah_model_missW_addY_model5, validation_data_model5)

mah_model_corrW_recalibrate_model5_results<-proposed_model_validation_results(mah_model_corrW_recalibrate_model5, validation_data_model5)

mah_model_missW_recalibrate_model5_results<-proposed_model_validation_results(mah_model_missW_recalibrate_model5, validation_data_model5)


#target only model validation
target_only_model_model5_results<-proposed_model_validation_results(target_only_model_model5, validation_data_model5)

#calibration models validation
intercept_calibration_full_model5_results<-calibrated_model_validation_results(intercept_calibration_full_model5, type='intercept_only', validation_data_model5 )

intercept_calibration_source_only_model5_results<-calibrated_model_validation_results(intercept_calibration_source_only_model5, type='intercept_only', validation_data_model5 )

logistic_calibration_full_model5_results<-calibrated_model_validation_results(logistic_calibration_full_model5, type='logistic', validation_data_model5 )

logistic_calibration_source_only_model5_results<-calibrated_model_validation_results(logistic_calibration_source_only_model5, type='logistic', validation_data_model5 )


#dynamic model validation
theta_updated_model5_results <- dynamic_model_validation_results(validation_data_model5 , theta_updated_model5)


########dataset model 4 predictors##################

#WEIGHTS
prop_weight_corr_inY_model4 <- propensity_weighting_with_lambda(source_target_model4, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_corr_exY_model4 <- propensity_weighting_with_lambda(source_target_model4, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

prop_weight_miss_inY_model4 <- propensity_weighting_with_lambda(source_target_model4, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
prop_weight_miss_exY_model4 <- propensity_weighting_with_lambda(source_target_model4, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_corr_inY_model4 <-  mahalanobis_weighting_with_lambda(source_target_model4, misspec='correct-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_corr_exY_model4 <-  mahalanobis_weighting_with_lambda(source_target_model4, misspec='correct-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

mah_weight_miss_inY_model4 <-  mahalanobis_weighting_with_lambda(source_target_model4, misspec='miss-spec', include_Y='include', lambda_hyperparam_values, folds_num, iteration)
mah_weight_miss_exY_model4 <-  mahalanobis_weighting_with_lambda(source_target_model4, misspec='miss-spec', include_Y='exclude', lambda_hyperparam_values, folds_num, iteration)

# MODELS

#propensity models
propensity_model_corrW_addY_model4 <- weighted_LR(source_target_model4, prop_weight_corr_inY_model4,  included_Y_in_weights='include')
propensity_model_corrW_recalibrate_model4 <- weighted_LR(source_target_model4, prop_weight_corr_exY_model4,  included_Y_in_weights='exclude')

propensity_model_missW_addY_model4 <- weighted_LR(source_target_model4, prop_weight_miss_inY_model4,  included_Y_in_weights='include')
propensity_model_missW_recalibrate_model4 <- weighted_LR(source_target_model4, prop_weight_miss_exY_model4,  included_Y_in_weights='exclude')

#distance models
mah_model_corrW_addY_model4 <- weighted_LR(source_target_model4, mah_weight_corr_inY_model4,  included_Y_in_weights='include')
mah_model_corrW_recalibrate_model4 <- weighted_LR(source_target_model4, mah_weight_corr_exY_model4,  included_Y_in_weights='exclude')

mah_model_missW_addY_model4 <- weighted_LR(source_target_model4, mah_weight_miss_inY_model4,  included_Y_in_weights='include')
mah_model_missW_recalibrate_model4 <- weighted_LR(source_target_model4, mah_weight_miss_exY_model4,  included_Y_in_weights='exclude')



#updating methods models
logistic_calibration_full_model4<- logistic_calibration(source_target_model4, development_data='FULL')
logistic_calibration_source_only_model4 <- logistic_calibration(source_target_model4, development_data='source_only')


intercept_calibration_full_model4<- intercept_calibration(source_target_model4, development_data='FULL') #make predictions with predict(oo1, type='response')
intercept_calibration_source_only_model4 <- intercept_calibration(source_target_model4, development_data='source_only')

#target data only model
target_only_model_model4 <- target_only_LR(source_target_model4)

#dynamic logistic model
theta_updated_model4 <- dynamic_model_theta_with_lambda(source_target_model4,  lambda_hyperparam_values, folds_num, iteration)

##VALIDATION

#propensity models validation
propensity_model_corrW_addY_model4_results<-proposed_model_validation_results(propensity_model_corrW_addY_model4, validation_data_model4)

propensity_model_missW_addY_model4_results<-proposed_model_validation_results(propensity_model_missW_addY_model4, validation_data_model4)

propensity_model_corrW_recalibrate_model4_results<-proposed_model_validation_results(propensity_model_corrW_recalibrate_model4, validation_data_model4)

propensity_model_missW_recalibrate_model4_results<-proposed_model_validation_results(propensity_model_missW_recalibrate_model4, validation_data_model4)

#ditance models validation
mah_model_corrW_addY_model4_results<-proposed_model_validation_results(mah_model_corrW_addY_model4, validation_data_model4)

mah_model_missW_addY_model4_results<-proposed_model_validation_results(mah_model_missW_addY_model4, validation_data_model4)

mah_model_corrW_recalibrate_model4_results<-proposed_model_validation_results(mah_model_corrW_recalibrate_model4, validation_data_model4)

mah_model_missW_recalibrate_model4_results<-proposed_model_validation_results(mah_model_missW_recalibrate_model4, validation_data_model4)


#target only model validation
target_only_model_model4_results<-proposed_model_validation_results(target_only_model_model4, validation_data_model4)

#calibration models validation
intercept_calibration_full_model4_results<-calibrated_model_validation_results(intercept_calibration_full_model4, type='intercept_only', validation_data_model4 )

intercept_calibration_source_only_model4_results<-calibrated_model_validation_results(intercept_calibration_source_only_model4, type='intercept_only', validation_data_model4 )

logistic_calibration_full_model4_results<-calibrated_model_validation_results(logistic_calibration_full_model4, type='logistic', validation_data_model4 )

logistic_calibration_source_only_model4_results<-calibrated_model_validation_results(logistic_calibration_source_only_model4, type='logistic', validation_data_model4 )


#dynamic model validation
theta_updated_model4_results <- dynamic_model_validation_results(validation_data_model4 , theta_updated_model4)


val_metrics <- bind_rows(list(
                              propensity_model_corrW_addY_model15= propensity_model_corrW_addY_model15_results$val_results,
                              propensity_model_missW_addY_model15= propensity_model_missW_addY_model15_results$val_results,
                              propensity_model_corrW_recalibrate_model15= propensity_model_corrW_recalibrate_model15_results$val_results,
                              propensity_model_missW_recalibrate_model15= propensity_model_missW_recalibrate_model15_results$val_results,
                              mah_model_corrW_addY_model15= mah_model_corrW_addY_model15_results$val_results,
                              mah_model_missW_addY_model15= mah_model_missW_addY_model15_results$val_results,
                              mah_model_corrW_recalibrate_model15= mah_model_corrW_recalibrate_model15_results$val_results,
                              mah_model_missW_recalibrate_model15= mah_model_missW_recalibrate_model15_results$val_results,
                              target_only_model_model15 =  target_only_model_model15_results$val_results,
                              intercept_calibration_full_model15 = intercept_calibration_full_model15_results$val_results,
                              intercept_calibration_source_only_model15= intercept_calibration_source_only_model15_results$val_results,
                              logistic_calibration_full_model15 = logistic_calibration_full_model15_results$val_results,
                              logistic_calibration_source_only_model15= logistic_calibration_source_only_model15_results$val_results,
                              bayesian_updating_model15= theta_updated_model15_results$val_results,   
  
                              propensity_model_corrW_addY_model5= propensity_model_corrW_addY_model5_results$val_results,
                              propensity_model_missW_addY_model5= propensity_model_missW_addY_model5_results$val_results,
                              propensity_model_corrW_recalibrate_model5= propensity_model_corrW_recalibrate_model5_results$val_results,
                              propensity_model_missW_recalibrate_model5= propensity_model_missW_recalibrate_model5_results$val_results,
                              mah_model_corrW_addY_model5= mah_model_corrW_addY_model5_results$val_results,
                              mah_model_missW_addY_model5= mah_model_missW_addY_model5_results$val_results,
                              mah_model_corrW_recalibrate_model5= mah_model_corrW_recalibrate_model5_results$val_results,
                              mah_model_missW_recalibrate_model5= mah_model_missW_recalibrate_model5_results$val_results,
                              target_only_model_model5 =  target_only_model_model5_results$val_results,
                              intercept_calibration_full_model5 = intercept_calibration_full_model5_results$val_results,
                              intercept_calibration_source_only_model5= intercept_calibration_source_only_model5_results$val_results,
                              logistic_calibration_full_model5 = logistic_calibration_full_model5_results$val_results,
                              logistic_calibration_source_only_model5= logistic_calibration_source_only_model5_results$val_results,
                              bayesian_updating_model5= theta_updated_model5_results$val_results, 
                              
                              propensity_model_corrW_addY_model4= propensity_model_corrW_addY_model4_results$val_results,
                              propensity_model_missW_addY_model4= propensity_model_missW_addY_model4_results$val_results,
                              propensity_model_corrW_recalibrate_model4= propensity_model_corrW_recalibrate_model4_results$val_results,
                              propensity_model_missW_recalibrate_model4= propensity_model_missW_recalibrate_model4_results$val_results,
                              mah_model_corrW_addY_model4= mah_model_corrW_addY_model4_results$val_results,
                              mah_model_missW_addY_model4= mah_model_missW_addY_model4_results$val_results,
                              mah_model_corrW_recalibrate_model4= mah_model_corrW_recalibrate_model4_results$val_results,
                              mah_model_missW_recalibrate_model4= mah_model_missW_recalibrate_model4_results$val_results,
                              target_only_model_model4 =  target_only_model_model4_results$val_results,
                              intercept_calibration_full_model4 = intercept_calibration_full_model4_results$val_results,
                              intercept_calibration_source_only_model4= intercept_calibration_source_only_model4_results$val_results,
                              logistic_calibration_full_model4 = logistic_calibration_full_model4_results$val_results,
                              logistic_calibration_source_only_model4= logistic_calibration_source_only_model4_results$val_results,
                              bayesian_updating_model4= theta_updated_model4_results$val_results ), .id = "Model")

cal_plot <- bind_rows(list(
  propensity_model_corrW_addY_model15= propensity_model_corrW_addY_model15_results$cal_plot,
  propensity_model_missW_addY_model15= propensity_model_missW_addY_model15_results$cal_plot,
  propensity_model_corrW_recalibrate_model15= propensity_model_corrW_recalibrate_model15_results$cal_plot,
  propensity_model_missW_recalibrate_model15= propensity_model_missW_recalibrate_model15_results$cal_plot,
  mah_model_corrW_addY_model15= mah_model_corrW_addY_model15_results$cal_plot,
  mah_model_missW_addY_model15= mah_model_missW_addY_model15_results$cal_plot,
  mah_model_corrW_recalibrate_model15= mah_model_corrW_recalibrate_model15_results$cal_plot,
  mah_model_missW_recalibrate_model15= mah_model_missW_recalibrate_model15_results$cal_plot,
  target_only_model_model15 =  target_only_model_model15_results$cal_plot,
  intercept_calibration_full_model15 = intercept_calibration_full_model15_results$cal_plot,
  intercept_calibration_source_only_model15= intercept_calibration_source_only_model15_results$cal_plot,
  logistic_calibration_full_model15 = logistic_calibration_full_model15_results$cal_plot,
  logistic_calibration_source_only_model15= logistic_calibration_source_only_model15_results$cal_plot,
  bayesian_updating_model15= theta_updated_model15_results$cal_plot,   
  
  propensity_model_corrW_addY_model5= propensity_model_corrW_addY_model5_results$cal_plot,
  propensity_model_missW_addY_model5= propensity_model_missW_addY_model5_results$cal_plot,
  propensity_model_corrW_recalibrate_model5= propensity_model_corrW_recalibrate_model5_results$cal_plot,
  propensity_model_missW_recalibrate_model5= propensity_model_missW_recalibrate_model5_results$cal_plot,
  mah_model_corrW_addY_model5= mah_model_corrW_addY_model5_results$cal_plot,
  mah_model_missW_addY_model5= mah_model_missW_addY_model5_results$cal_plot,
  mah_model_corrW_recalibrate_model5= mah_model_corrW_recalibrate_model5_results$cal_plot,
  mah_model_missW_recalibrate_model5= mah_model_missW_recalibrate_model5_results$cal_plot,
  target_only_model_model5 =  target_only_model_model5_results$cal_plot,
  intercept_calibration_full_model5 = intercept_calibration_full_model5_results$cal_plot,
  intercept_calibration_source_only_model5= intercept_calibration_source_only_model5_results$cal_plot,
  logistic_calibration_full_model5 = logistic_calibration_full_model5_results$cal_plot,
  logistic_calibration_source_only_model5= logistic_calibration_source_only_model5_results$cal_plot,
  bayesian_updating_model5= theta_updated_model5_results$cal_plot, 
  
  propensity_model_corrW_addY_model4= propensity_model_corrW_addY_model4_results$cal_plot,
  propensity_model_missW_addY_model4= propensity_model_missW_addY_model4_results$cal_plot,
  propensity_model_corrW_recalibrate_model4= propensity_model_corrW_recalibrate_model4_results$cal_plot,
  propensity_model_missW_recalibrate_model4= propensity_model_missW_recalibrate_model4_results$cal_plot,
  mah_model_corrW_addY_model4= mah_model_corrW_addY_model4_results$cal_plot,
  mah_model_missW_addY_model4= mah_model_missW_addY_model4_results$cal_plot,
  mah_model_corrW_recalibrate_model4= mah_model_corrW_recalibrate_model4_results$cal_plot,
  mah_model_missW_recalibrate_model4= mah_model_missW_recalibrate_model4_results$cal_plot,
  target_only_model_model4 =  target_only_model_model4_results$cal_plot,
  intercept_calibration_full_model4 = intercept_calibration_full_model4_results$cal_plot,
  intercept_calibration_source_only_model4= intercept_calibration_source_only_model4_results$cal_plot,
  logistic_calibration_full_model4 = logistic_calibration_full_model4_results$cal_plot,
  logistic_calibration_source_only_model4= logistic_calibration_source_only_model4_results$cal_plot,
  bayesian_updating_model4= theta_updated_model4_results$cal_plot ), .id = "Model") %>% group_by(Model) %>%  summarise(
    sm = list(sm))
#    Sm.full = list(Sm.full)
#    ) 


indv_pred <- bind_rows(list(
  propensity_model_corrW_addY_model15= propensity_model_corrW_addY_model15_results$preds,
  propensity_model_missW_addY_model15= propensity_model_missW_addY_model15_results$preds,
  propensity_model_corrW_recalibrate_model15= propensity_model_corrW_recalibrate_model15_results$preds,
  propensity_model_missW_recalibrate_model15= propensity_model_missW_recalibrate_model15_results$preds,
  mah_model_corrW_addY_model15= mah_model_corrW_addY_model15_results$preds,
  mah_model_missW_addY_model15= mah_model_missW_addY_model15_results$preds,
  mah_model_corrW_recalibrate_model15= mah_model_corrW_recalibrate_model15_results$preds,
  mah_model_missW_recalibrate_model15= mah_model_missW_recalibrate_model15_results$preds,
  target_only_model_model15 =  target_only_model_model15_results$preds,
  intercept_calibration_full_model15 = intercept_calibration_full_model15_results$preds,
  intercept_calibration_source_only_model15= intercept_calibration_source_only_model15_results$preds,
  logistic_calibration_full_model15 = logistic_calibration_full_model15_results$preds,
  logistic_calibration_source_only_model15= logistic_calibration_source_only_model15_results$preds,
  theta_updated_model15= theta_updated_model15_results$preds,   
  
  propensity_model_corrW_addY_model5= propensity_model_corrW_addY_model5_results$preds,
  propensity_model_missW_addY_model5= propensity_model_missW_addY_model5_results$preds,
  propensity_model_corrW_recalibrate_model5= propensity_model_corrW_recalibrate_model5_results$preds,
  propensity_model_missW_recalibrate_model5= propensity_model_missW_recalibrate_model5_results$preds,
  mah_model_corrW_addY_model5= mah_model_corrW_addY_model5_results$preds,
  mah_model_missW_addY_model5= mah_model_missW_addY_model5_results$preds,
  mah_model_corrW_recalibrate_model5= mah_model_corrW_recalibrate_model5_results$preds,
  mah_model_missW_recalibrate_model5= mah_model_missW_recalibrate_model5_results$preds,
  target_only_model_model5 =  target_only_model_model5_results$preds,
  intercept_calibration_full_model5 = intercept_calibration_full_model5_results$preds,
  intercept_calibration_source_only_model5= intercept_calibration_source_only_model5_results$preds,
  logistic_calibration_full_model5 = logistic_calibration_full_model5_results$preds,
  logistic_calibration_source_only_model5= logistic_calibration_source_only_model5_results$preds,
  theta_updated_model5= theta_updated_model5_results$preds, 
  
  propensity_model_corrW_addY_model4= propensity_model_corrW_addY_model4_results$preds,
  propensity_model_missW_addY_model4= propensity_model_missW_addY_model4_results$preds,
  propensity_model_corrW_recalibrate_model4= propensity_model_corrW_recalibrate_model4_results$preds,
  propensity_model_missW_recalibrate_model4= propensity_model_missW_recalibrate_model4_results$preds,
  mah_model_corrW_addY_model4= mah_model_corrW_addY_model4_results$preds,
  mah_model_missW_addY_model4= mah_model_missW_addY_model4_results$preds,
  mah_model_corrW_recalibrate_model4= mah_model_corrW_recalibrate_model4_results$preds,
  mah_model_missW_recalibrate_model4= mah_model_missW_recalibrate_model4_results$preds,
  target_only_model_model4 =  target_only_model_model4_results$preds,
  intercept_calibration_full_model4 = intercept_calibration_full_model4_results$preds,
  intercept_calibration_source_only_model4= intercept_calibration_source_only_model4_results$preds,
  logistic_calibration_full_model4 = logistic_calibration_full_model4_results$preds,
  logistic_calibration_source_only_model4= logistic_calibration_source_only_model4_results$preds,
  theta_updated_model4= theta_updated_model4_results$preds ), .id = "Model") 

transposed_indv_pred <- t(indv_pred)
colnames(transposed_indv_pred) <-  c( 91824, 34403, 62800, 41490, 74248, 25010, 85842, 71263, 79908)

if (!dir.exists("raw_results/cal_plot/")) {
  dir.create("raw_results/cal_plot/", recursive = TRUE)
}

if (!dir.exists("raw_results/validation/")) {
  dir.create("raw_results/validation/", recursive = TRUE)
}

if (!dir.exists("raw_results/preds/")) {
  dir.create("raw_results/preds/", recursive = TRUE)
}



saveRDS(cal_plot, paste0('raw_results/cal_plot/cal_plot_results_ID', scenario_ID, '_IT', iteration, "_TS", target_split, "_DS", dev_samp_size, '.rds'))

write.csv(val_metrics, paste0('raw_results/validation/validation_results_ID', scenario_ID, '_IT', iteration, "_TS", target_split, "_DS", dev_samp_size, '.csv'), row.names = FALSE)
write.csv(transposed_indv_pred, paste0('raw_results/preds/indv_preds_results_ID', scenario_ID, '_IT', iteration, "_TS", target_split, "_DS", dev_samp_size, '.csv'), row.names = TRUE)



end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
