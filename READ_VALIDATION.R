
validation_data_model15 <- readRDS( 'validation_data_model15.rds')

validation_data_model5 <- validation_data_model15[ c('X1','X2','X10','X11','U', 'Y', 'isSource' )] #readRDS("~/scratch/simulations_3001/#validation_data_model5.rds")

validation_data_model4 <- validation_data_model15[ c('X1','X2','X3','U', 'Y', 'isSource' )]#readRDS("~/scratch/simulations_3001/validation_data_model4.rds")



# 
# validation_data_model15 <- generate_validation(VAL_SAMP_SIZE, binary_predictors,
#                                 means_target, CORR_MATRIX,
#                                 binary_prev_target, predictors_names,
#                                 B_coeffcients_target_model15)
# 
# saveRDS(validation_data_model15, 'validation_data_model152.rds')
