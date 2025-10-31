CORR_MATRIX <- as.matrix(read.csv("corr_matrix.csv"))

# Data frame for model_15
MODEL_15 <- data.frame(
  model = "model_15",
  X1_m = 0, X1_v = 1, X2_m = 0, X2_v = 1, X3_m = 0, X3_v = 1, X4_m = 0, X4_v = 1, 
  X5_m = 0, X5_v = 1, X6_m = 0, X6_v = 1, X7_m = 0, X7_v = 1, X8_m = 0, X8_v = 1, 
  X9_m = 0, X9_v = 1, X10_m = 0, X10_v = 1, X11_m = 0, X11_v = 1, X12_m = 0, X12_v = 1, 
  X13_m = 0, X13_v = 1, X14_m = 0, X14_v = 1, U_m = 0, U_v = 1, X10_p = 0.3, X11_p = 0.4, 
  X12_p = 0.1, X13_p = 0.2, X14_p = 0.4, B0 = -3.4, B1 = 0.5, B2 = 0.3, B3 = 0.2, B4 = 0.1, 
  B5 = -0.02, B6 = 0.1, B7 = -0.1, B8 = 0.15, B9 = -0.1, B10 = 0.7, B11 = 0.8, B12 = 0.5, 
  B13 = 0.6, B14 = 0.2, BU = 0.2
)

# Data frame for model_5
MODEL_5 <- data.frame(
  model = "model_5",
  X1_m = 0, X1_v = 1, X2_m = 0, X2_v = 1, X3_m = 0, X3_v = 1, X4_m = 0, X4_v = 1, 
  X5_m = 0, X5_v = 1, X6_m = 0, X6_v = 1, X7_m = 0, X7_v = 1, X8_m = 0, X8_v = 1, 
  X9_m = 0, X9_v = 1, X10_m = 0, X10_v = 1, X11_m = 0, X11_v = 1, X12_m = 0, X12_v = 1, 
  X13_m = 0, X13_v = 1, X14_m = 0, X14_v = 1, U_m = 0, U_v = 1, X10_p = 0.3, X11_p = 0.4, 
  X12_p = 0.1, X13_p = 0.2, X14_p = 0.4, B0 = -3.1, B1 = 0.5, B2 = 0.3, B3 = 0, B4 = 0, 
  B5 = 0, B6 = 0, B7 = 0, B8 = 0, B9 = 0, B10 = 0.7, B11 = 0.8, B12 = 0, B13 = 0, B14 = 0, BU = 0.2
)

# Data frame for model_4
MODEL_4 <- data.frame(
  model = "model_4",
  X1_m = 0, X1_v = 1, X2_m = 0, X2_v = 1, X3_m = 0, X3_v = 1, X4_m = 0, X4_v = 1, 
  X5_m = 0, X5_v = 1, X6_m = 0, X6_v = 1, X7_m = 0, X7_v = 1, X8_m = 0, X8_v = 1, 
  X9_m = 0, X9_v = 1, X10_m = 0, X10_v = 1, X11_m = 0, X11_v = 1, X12_m = 0, X12_v = 1, 
  X13_m = 0, X13_v = 1, X14_m = 0, X14_v = 1, U_m = 0, U_v = 1, X10_p = 0.3, X11_p = 0.4, 
  X12_p = 0.1, X13_p = 0.2, X14_p = 0.4, B0 = -2.5, B1 = 0.5, B2 = 0.3, B3 = 0.2, B4 = 0, 
  B5 = 0, B6 = 0, B7 = 0, B8 = 0, B9 = 0, B10 = 0, B11 = 0, B12 = 0, B13 = 0, B14 = 0, BU = 0.2
)



means_target <- as.double(MODEL_15[grep("_m", colnames(MODEL_15), value = TRUE)])
binary_prev_target <- as.double(MODEL_15[grep("_p", colnames(MODEL_15), value = TRUE)])
B_coeffcients_target_model15 <-  as.double(MODEL_15[grep("B", colnames(MODEL_15), value = TRUE)])
B_coeffcients_target_model5 <-  as.double(MODEL_5[grep("B", colnames(MODEL_5), value = TRUE)])
B_coeffcients_target_model4 <-  as.double(MODEL_4[grep("B", colnames(MODEL_4), value = TRUE)])



N_SAMPLES <- 100000
VAL_SAMP_SIZE <- 100000
predictors_names <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','U' )
total_predictors <- length(predictors_names)
binary_predictors <- c('X10','X11','X12','X13','X14')
lambda_hyperparam_values <- c(seq(0, 1, by = 0.1), 0.05, 0.08, 0.15, 0.25, 0.03)
folds_num=4

