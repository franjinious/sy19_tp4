f_clas_perf <- function(test_class, pred_class){
  local.n.test <- length(test_class)
  local.n.pred <- length(pred_class)
  
  if(local.n.test != local.n.pred){
    warning("Test set and Prediction set have different length !")
    return(NULL)
  }
  
  levels(pred_class)  <- levels(test_class)
  local.perf          <- table(test_class, pred_class)
  
  local.error.total        <- 1 - sum (diag(local.perf)) / local.n.test
  local.error.within_class <- 1 -      diag(local.perf)  / rowSums(local.perf)
  
  # Return the results as a list
  res <- list(contingency.matrix      = local.perf,
              test.error.total        = local.error.total,
              test.error.within_class = local.error.within_class)
  return(res)
}

message("'f_clas_perf' is imported")