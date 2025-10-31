covarinace_matrix <- function(corr_mat, delta_v){
  diag(corr_mat) <- diag(corr_mat)+delta_v
  std <- sqrt(diag(corr_mat))
  cm <- corr_mat * outer(std, std)
  return(cm)
}


to_binray <- function(variable, prevalence, iteration){
  set.seed(12344*iteration)
  # Determine threshold value
  threshold <- quantile(variable, 1 - prevalence)
  # Convert continuous variable to binary
  binary_variable <- ifelse(variable >= threshold, 1, 0)
  return(binary_variable)
}


generate_covariates_source<-function(N, binary_predictors, means, means_delta, corr_mat,
                                     variances_delta , binary_prev, binary_prev_delta, colnames, iteration){
  set.seed(12344*iteration)
  
  
  corr_mat <- covarinace_matrix(corr_mat, variances_delta)
  means <- (means+means_delta)
  binary_prev <- binary_prev+binary_prev_delta
  
  
  data <- as.data.frame(mvrnorm(n=N,
                                mu=means,
                                Sigma=corr_mat ))
  colnames(data) <- colnames
  
  to_binary_variables <- mapply(to_binray, data[, binary_predictors], binary_prev, iteration ) #apply binary conversion function
  data[, binary_predictors ] <- as.data.frame(to_binary_variables) #pass result to dataframe
  
  return(data)
}


generate_covariates_target<-function(N, binary_predictors, means, corr_mat,
                                     binary_prev, colnames, iteration, dataset_type){
  if(dataset_type=="target"){
    set.seed(12345*iteration) #target
  }else{
    set.seed(12346*iteration) #validation
  }
  
  data <- as.data.frame(mvrnorm(n=N,
                                mu=means,
                                Sigma=corr_mat ))
  colnames(data) <- colnames
  
  to_binary_variables <- mapply(to_binray, data[, binary_predictors], binary_prev, iteration ) #apply binary conversion function
  data[, binary_predictors ] <- as.data.frame(to_binary_variables) #pass result to dataframe
  
  return(data)
}


generate_outcome_source <- function(covariates, coeffs_target, coeffs_delta, iteration ){
  set.seed(12344*iteration)
  
  mat <- covariates  %>% data.matrix()
  
  coeffs <- coeffs_target + coeffs_delta 
  #print("source coef")
  #print(coeffs)
  alpha <- coeffs[1]
  
  xb <- alpha + (mat%*%coeffs[-1])
  p <-  1/(1 + exp(-xb))
  y <- rbinom(n = nrow(covariates), size = 1, prob = p)
  #print(table(y)/nrow(covariates))
  
  drop_cols <- which(coeffs[-1] == 0)
  #print(drop_cols)
  if(length(drop_cols) > 0){
    covariates<-covariates %>% dplyr::select(-all_of(drop_cols))
  }
  covariates$Y <- y
  return(covariates)
}


generate_outcome_target <- function(covariates, coeffs_target, iteration, dataset_type){
  if(dataset_type=="target"){
    set.seed(12345*iteration) #target
  }else{
    set.seed(12346*iteration) #validation
  }
  
  mat <- covariates  %>% data.matrix()
  
  coeffs <- coeffs_target
  #print("target coeffs")
  #print(coeffs)
  alpha <- coeffs[1]
  
  xb <- alpha + (mat%*%coeffs[-1])
  p <-  1/(1 + exp(-xb))
  y <- rbinom(n = nrow(covariates), size = 1, prob = p)
  
  drop_cols <- which(coeffs[-1] == 0)
  
  if(length(drop_cols) > 0){
    covariates<-covariates %>% dplyr::select(-all_of(drop_cols))
  }
  covariates$Y <- y
  return(covariates)
}

sampsize_calc <- function(data, predictors_names){
  set.seed(12344)
  source <- data$source
  target <- data$target
  
  df <- rbind(source, target)
  df <- df %>%dplyr::select(any_of(c(predictors_names, 'Y')))
  prev = prop.table(table(df$Y))[2]
  model <- glm(Y~. , data=df, family = 'binomial')
  R2 <- PseudoR2(model, which = 'CoxSnell')
  predictors <-  ncol(df%>% dplyr::select(-Y))
  result <- pmsampsize(type = "b",shrinkage=0.9, csrsquared = R2, parameters = predictors, prevalence = prev)
  return(result$sample_size)
}


generate_development <- function( N_samples, target_split, binary_predictors,
                                  means_target, means_delta, corr_matrix, variances_delta,
                                  binary_prev_target, binary_prev_delta, predictors_names, 
                                  B_coeffcients_target, B_coeffcients_delta, iteration){
  
  source_n <- round(N_samples*(1-target_split)) 
  target_n <- round(N_samples*target_split)
  
  source <- generate_covariates_source(source_n, binary_predictors,
                                       means_target, means_delta, corr_matrix, variances_delta,
                                       binary_prev_target, binary_prev_delta, predictors_names, iteration)
  
  target <- generate_covariates_target(target_n, binary_predictors, means_target, corr_matrix,
                                       binary_prev_target, predictors_names, iteration, "target")
  
  
  source_with_outcome <- generate_outcome_source(source, B_coeffcients_target,  B_coeffcients_delta,iteration)
  target_with_outcome <- generate_outcome_target(target, B_coeffcients_target, iteration, "target")
  source_with_outcome$isSource <- 1
  target_with_outcome$isSource <- 0
  
  full_data <- list(source=source_with_outcome, target=target_with_outcome)
  return(full_data)
}


generate_validation <- function( N_samples, binary_predictors,
                                 means_target, corr_matrix,
                                 binary_prev_target, predictors_names, 
                                 B_coeffcients_target){
  iteration <- 1
  target <- generate_covariates_target(N_samples , binary_predictors, means_target, corr_matrix,
                                       binary_prev_target, predictors_names, iteration, "validation")
  
  target_with_outcome <- generate_outcome_target(target, B_coeffcients_target, iteration, "validation")
  target_with_outcome$isSource <- 0
  
  return(target_with_outcome)
}

propensity_weighting<- function(df, misspec, include_Y){
  source <- df$source
  target <- df$target
  
  if(misspec=='miss-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y, U))
    target <- target%>% dplyr::select(-c(Y, U))
  }else if(misspec=='miss-spec' & include_Y=='include' ){
    source <- source%>% dplyr::select(-U)
    target <- target%>% dplyr::select(-U)
  }else if(misspec=='correct-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-Y)
    target <- target%>% dplyr::select(-Y)
  }
  temp <- rbind(source, target)
  
  membership_model <- glm(isSource~., data=temp, family = 'binomial')
  #print(membership_model)
  score <- predict(membership_model,type="response", newdata = source) #predict p(isSource=1|X)
  
  propensity_weight <-  (1 - score)/ score #p(p(isSource=0|X)/ p(isSource=1|X))
  #print(propensity_weight)
  
  propensity_weight <- propensity_weight*(nrow(source)/ nrow(target))
  
  propensity_weight[propensity_weight >= 1]= 1
  source$propensity_weight <- propensity_weight
  target$propensity_weight <- 1
  
  weights <- list(source=source$propensity_weight, target=target$propensity_weight )
  return(weights)
}


propensity_weighting_with_lambda<- function(df, misspec, include_Y, hyperparam_values, folds_num, iteration){
  source <- df$source
  target <- df$target
  
  if(misspec=='miss-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y, U))
    target <- target%>% dplyr::select(-c(Y, U))
  }else if(misspec=='miss-spec' & include_Y=='include' ){
    source <- source%>% dplyr::select(-U)
    target <- target%>% dplyr::select(-U)
  }else if(misspec=='correct-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-Y)
    target <- target%>% dplyr::select(-Y)
  }
  temp <- rbind(source, target)
  membership_model <- glm(isSource~., data=temp, family = 'binomial')
  #print(membership_model)
  score <- predict(membership_model,type="response", newdata = source) #predict p(isSource=1|X)
  
  propensity_weight <-  (1 - score)/ score #p(p(isSource=0|X)/ p(isSource=1|X))
  #print(propensity_weight)
  
  propensity_weight <- propensity_weight*(nrow(source)/ nrow(target))
  
  propensity_weight[propensity_weight >= 1]= 1
  source$propensity_weight <- propensity_weight
  target$propensity_weight <- 1
  
  lambda <- find_lambda(df, hyperparam_values, folds_num, misspec, include_Y, 'propensity', iteration)
  print(lambda)
  weights_adjusted <- c(source$propensity_weight*lambda,target$propensity_weight )
  return(weights_adjusted)
}

mahalanobis_weighting_with_lambda <- function(df, misspec, include_Y, hyperparam_values, folds_num, iteration){
  source <- df$source
  target <- df$target
  
  if(misspec=='miss-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y, U, isSource))
    target <- target%>% dplyr::select(-c(Y, U, isSource))
  }else if(misspec=='miss-spec' & include_Y=='include' ){
    source <- source%>% dplyr::select(-c(U, isSource))
    target <- target%>% dplyr::select(-c(U, isSource))
  }else if(misspec=='correct-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y,isSource))
    target <- target%>% dplyr::select(-c(Y,isSource))
  }else{
    source <- source%>% dplyr::select(-isSource)
    target <- target%>% dplyr::select(-isSource)
  }
  
  source <- data.frame(lapply( source, as.numeric))
  target <- data.frame(lapply( target, as.numeric))
  
  tryCatch(
    {distances <- mahalanobis(source, colMeans(target), cov(target)) #get mahalanobis distanes
    
    fx <-  replicate(nrow(source), 1) #fx uniform disrtibution
    pvalues <- pchisq(distances, df=ncol(source), lower.tail=FALSE) #get pvalues of disnces from chi-square distribution
    
    gx<- density(x=pvalues) #estimate gx
    gx_prob <- approxfun(gx$x, gx$y) #get gx probabilities
    
    source_weights <- fx/gx_prob(pvalues)
    #print(source_weights)
    source_weights[source_weights > 1]= 1
    
    source$mah_weight <- source_weights
    target$mah_weight <- 1
    
    lambda <- find_lambda(df, hyperparam_values, folds_num, misspec, include_Y, 'distance', iteration)
    print(lambda)
    weights_adjusted <- c(source$mah_weight*lambda,target$mah_weight )
    
    return(weights_adjusted)
    }, error=function(e){
      return(c(NA, NA, NA, NA)) 
    }
  )
}

mahalanobis_weighting <- function(df, misspec, include_Y){
  source <- df$source
  target <- df$target
  
  if(misspec=='miss-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y, U, isSource))
    target <- target%>% dplyr::select(-c(Y, U, isSource))
  }else if(misspec=='miss-spec' & include_Y=='include' ){
    source <- source%>% dplyr::select(-c(U, isSource))
    target <- target%>% dplyr::select(-c(U, isSource))
  }else if(misspec=='correct-spec' & include_Y=='exclude' ){
    source <- source%>% dplyr::select(-c(Y,isSource))
    target <- target%>% dplyr::select(-c(Y,isSource))
  }else{
    source <- source%>% dplyr::select(-isSource)
    target <- target%>% dplyr::select(-isSource)
  }
  
  source <- data.frame(lapply( source, as.numeric))
  target <- data.frame(lapply( target, as.numeric))
  
  
  distances <- mahalanobis(source, colMeans(target), cov(target)) #get mahalanobis distanes
  
  fx <-  replicate(nrow(source), 1) #fx uniform disrtibution
  pvalues <- pchisq(distances, df=ncol(source), lower.tail=FALSE) #get pvalues of disnces from chi-square distribution
  
  gx<- density(x=pvalues) #estimate gx
  gx_prob <- approxfun(gx$x, gx$y) #get gx probabilities
  
  source_weights <- fx/gx_prob(pvalues)
  #print(source_weights)
  source_weights[source_weights > 1]= 1
  
  source$mah_weight <- source_weights
  target$mah_weight <- 1
  
  
  weights <- list(source=source$mah_weight,target=target$mah_weight )
  
  return(weights)
}


# Define custom cross-validation function
custom_cv <- function(source, target, folds, iteration) {
  set.seed(12344*iteration)
  indices_source <- sample(rep(1:folds, length.out = nrow(source)))
  indices_target <- sample(rep(1:folds, length.out = nrow(target)))
  
  folds_list <- lapply(1:folds, function(i) {
    train_source_indices <- which(indices_source != i)
    train_target_indices <- which(indices_target != i)
    test_indices <- which(indices_target == i)
    list(train_source = train_source_indices, train_target= train_target_indices, test = test_indices)
  })
  return(folds_list)
}

# Define function to calculate weighted logistic regression
weighted_logistic_regression <- function(source_target_data, test_data, weight, included_Y_in_weights) {
  train_data <- rbind(source_target_data$source, source_target_data$target )
  if(included_Y_in_weights=='include'){
    train_data <- train_data %>% dplyr::select(-c(U, isSource))
    test_data <- test_data %>% dplyr::select(-c(U, isSource))
  }else{
    train_data <- train_data%>% dplyr::select(-c(U))
    test_data <- test_data%>% dplyr::select(-c(U))
  }
  #print(head(train_data, 1))
  model <- glm(formula = Y~., data = train_data, family = "binomial", weights = weight)
  #print(summary(model))
  pred_probs <- predict(model, newdata = test_data, type = "response")
  return( pred_probs)
}

# # Define function to calculate mean squared error
calculate_mse <- function(true_labels, pred_probs) {
  mse <- MLmetrics::MSE(y_pred = pred_probs, y_true = true_labels)
  return(mse)
}


find_lambda <- function(data, hyperparam_values, folds_num, misspec , include_Y, method, iteration){
  mse_results <- numeric(length(hyperparam_values))
  
  for (i in seq_along(hyperparam_values)) {
    lambda <- hyperparam_values[i]
    
    source <- data$source
    target <- data$target
    
    folds <- custom_cv(source, target, folds = folds_num, iteration) 
    mse_values <- numeric(length(folds))
    
    for (j in seq_along(folds)) {
      train_source_indices <- folds[[j]]$train_source
      train_target_indices <- folds[[j]]$train_target
      test_indices <- folds[[j]]$test
      
      train_source_data <- source[train_source_indices, ] 
      train_target_data <- target[train_target_indices, ] 
      test_data <- target[test_indices, ] 
      
      train_data <- list(source=train_source_data, target=train_target_data)
      
      if(method == 'propensity'){
        source_target_weights <- propensity_weighting(train_data, misspec, include_Y)
      }else if(method =='distance'){
        source_target_weights <- mahalanobis_weighting(train_data, misspec, include_Y)
      }
      
      source_weights<- source_target_weights$source*lambda
      weights <- c(source_weights, source_target_weights$target)
      result <- weighted_logistic_regression(train_data, test_data, weight = weights, included_Y_in_weights=include_Y)
      mse_values[j] <- calculate_mse(true_labels = test_data$Y, pred_probs = result)
      
    }
    
    mse_results[i] <- mean(mse_values)
  }
  
  
  optimal_param <- hyperparam_values[which.min(mse_results)]
  return(optimal_param)
}




weighted_LR <- function(source_target_data, weight, included_Y_in_weights) {
  if(all(is.na(weight))){
    return( NA)
  }else{
    train_data <- rbind(source_target_data$source, source_target_data$target )
    if(included_Y_in_weights=='include'){
      train_data <- train_data %>% dplyr::select(-c(U, isSource))
    }else{
      train_data <- train_data%>% dplyr::select(-c(U))
    }
    model <- glm(formula = Y~., data = train_data, family = "binomial", weights = weight)
    #print(summary(model))
    return( model)
  }
}


logistic_calibration <- function(source_target_data, development_data) {
  target <- source_target_data$target %>% dplyr::select(-c(U, isSource))
  if(development_data=='FULL'){
    train_data <- rbind(source_target_data$source, source_target_data$target ) %>% dplyr::select(-c(U, isSource))
  }else{
    train_data <- source_target_data$source %>% dplyr::select(-c(U, isSource))
  }
  
  source_model <- glm(Y~., data=train_data, family = 'binomial', x=TRUE, y=TRUE)
  
  #target_model <- glm(Y~., data=target, family = 'binomial', x=TRUE, y=TRUE)
  target$lp <- predict(source_model, newdata=target) #get Lp of source model on target data
  calibrated_model <- glm(Y~lp, data=target, family='binomial',x=T, y=T)#update all model's coeff
  
  #print(exp(calibrated_model$coefficients))
  
  return(list(calibrated_model=calibrated_model, source_model=source_model))
} 

intercept_calibration <- function(source_target_data, development_data) {
  target <- source_target_data$target %>% dplyr::select(-c(U, isSource))
  if(development_data=='FULL'){
    train_data <- rbind(source_target_data$source, source_target_data$target ) %>% dplyr::select(-c(U, isSource))
  }else{
    train_data <- source_target_data$source %>% dplyr::select(-c(U, isSource))
  }
  
  source_model <- glm(Y~., data=train_data, family = 'binomial', x=TRUE, y=TRUE)
  
  #target_model <- glm(Y~., data=target, family = 'binomial', x=TRUE, y=TRUE)
  target$lp <- predict(source_model, newdata=target) #get Lp of source model on target data
  calibrated_model <- glm(Y~offset(lp), data=target, family='binomial',x=T, y=T)#update intercept only
  
  #print(predict(calibrated_model, newdata = target, type='response')[1:10])
  #print(exp(calibrated_model$coefficients))
  
  return(list(calibrated_model=calibrated_model, source_model=source_model))
  
}


target_only_LR <- function(source_target_data) {
  
  target <- source_target_data$target %>% dplyr::select(-c(U, isSource))
  
  model <- glm(formula = Y~., data = target, family = "binomial")
  #print(summary(model))
  return( model)
}



dynamic_model_theta <- function(data, lambda){
  initial_param <- source_parameters_dynamic(data$source)
  theta_updated <- update_parameters_dynamic(initial_param$theta, initial_param$sigma, data$target, lambda = lambda)
  return(theta_updated)
}

dynamic_model_theta_with_lambda <- function(data, hyperparam_values, folds_num, iteration){
  initial_param <- source_parameters_dynamic(data$source)
  lambda <- find_lambda_dynamic(data, hyperparam_values, folds_num, iteration)
  print(lambda)
  theta_updated <- update_parameters_dynamic(initial_param$theta, initial_param$sigma, data$target, lambda = lambda)
  return(theta_updated)
}

# Function to update parameters using Bayesian dynamic modeling
update_parameters_dynamic <- function(theta_hat_prev, Sigma_hat_prev, target, lambda) {
  target <- target%>% dplyr::select(-c(U, isSource))
  
  Xt <- target%>% dplyr::select(-Y)
  Yt <- target$Y
  
  Xt<-as.matrix(sapply(Xt, as.numeric))
  Xt <- cbind(1, Xt) # Add intercept
  
  theta_hat <- rep(0, ncol(Xt))
  #Rt <- Sigma_hat_prev / lambda
  #print(Sigma_hat_prev / lambda)
  tryCatch(
    {
      Rt <- Sigma_hat_prev
      #Rt <- (Rt + t(Rt))/2
      #Rt <- Rt / lambda
      diag(Rt) <- diag(Rt) / lambda
      y_hat <- (1 / (1 + exp(-Xt %*% theta_hat_prev)))
      
      y_hat_derivative <- matrix(rep(y_hat * (1 - y_hat), dim(Xt)[2]), nrow = dim(Xt)[2], byrow = TRUE)
      
      DL <- t(Xt) %*% (Yt - y_hat)
      D2L <- -solve(Rt) - (t(Xt)* y_hat_derivative ) %*% Xt  
      
      
      theta_hat <- theta_hat_prev - (solve(D2L) %*% DL)
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred: Lambda for Rt')
    }
  )
  # Update Sigma_hat
  #Sigma_hat <- solve(-D2L)
  return(theta_hat)
}


source_parameters_dynamic <- function(source) {
  source <- source%>% dplyr::select(-c(U,isSource))
  model <- glm(Y~., data = source, family = binomial(link = logit))
  #print(coefficients(model))
  return(list(theta = coefficients(model), sigma = vcov(model))) 
}


find_lambda_dynamic <- function(data, hyperparam_values, folds_num, iteration){
  mse_results <- numeric(length(hyperparam_values))
  
  for (i in seq_along(hyperparam_values)) {
    lambda <- hyperparam_values[i]
    
    source <- data$source
    target <- data$target
    
    folds <- custom_cv(source, target, folds = folds_num, iteration) 
    mse_values <- numeric(length(folds))
    
    for (j in seq_along(folds)) {
      train_source_indices <- folds[[j]]$train_source
      train_target_indices <- folds[[j]]$train_target
      test_indices <- folds[[j]]$test
      
      train_source_data <- source[train_source_indices, ] 
      train_target_data <- target[train_target_indices, ] 
      test_data <- target[test_indices, ] 
      
      train_data <- list(source=train_source_data, target=train_target_data)
      
      
      theta_updated <- dynamic_model_theta(train_data, lambda)
      if(all(theta_updated==0)){
        mse_values[j] <- NaN
      }
      else{
        result<- predict_prob_dynamic(test_data , theta_updated)
        mse_values[j] <- calculate_mse(true_labels = test_data$Y, pred_probs = result)
      }
      
    }
    
    mse_results[i] <- mean(mse_values)
  }
  optimal_param <- hyperparam_values[which.min(mse_results)]
  return(optimal_param)
}


predict_prob_dynamic <- function(val_data, theta_hat) {
  val_data <- val_data%>% dplyr::select(-c(U, isSource, Y))
  # Add intercept term to new_data
  Xt<-as.matrix(sapply(val_data, as.numeric))
  Xt <- cbind(1, Xt) # Add intercept
  # Compute predicted probabilities
  prob <- 1 / (1 + exp(-(Xt %*% theta_hat)))
  return(prob)
}


