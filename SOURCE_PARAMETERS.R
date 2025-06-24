#SOURCE.PARAM <- readRDS('SOURCE_PARAM_sampsize4_csf.rds')
SOURCE.PARAM <- readRDS('SOURCE_PARAM.rds')

get_variances_delta <- function(row){
  v_delta <- as.double(row[grep("v_delta", colnames(row), value=TRUE)])
  return(v_delta)
}

get_means_delta <- function(row){
  m_delta <- as.double(row[grep("_m_delta", colnames(row), value=TRUE)])
  return(m_delta)
}

get_binary_prev_delta <- function(row){
  p_delta <- as.double(row[grep("_p_delta", colnames(row), value=TRUE)])
  return(p_delta)
}

get_B_coeffcients_delta <- function(row){
  B_delta <- as.double(row[grep("B", colnames(row), value=TRUE)])
  return(B_delta)
}

get_dev_samp_size <- function(row){
  dev_samp_size <- as.double(row[grep("dev_samp_size", colnames(row), value=TRUE)])
  return(dev_samp_size)
}

get_target_split <- function(row){
  target_split <- as.double(row[grep("target_split", colnames(row), value=TRUE)])
  return(target_split)
}

get_iteration <- function(row){
  iter <- as.double(row[grep("iteration", colnames(row), value=TRUE)])
  return(iter)
}

get_ID <- function(row){
  id <- as.double(row[grep("ID", colnames(row), value=TRUE)])
  return(id)
}
