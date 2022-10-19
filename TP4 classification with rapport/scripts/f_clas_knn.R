if(!exists("f_clas_perf")) source("scripts/f_clas_perf.R")

f_clas_knn <- function(train, test, parameters = NULL){
  require("FNN")
  
  # Import parameters
  if(is.null(parameters)) {
    warning("The parameters 'k' is missing. Use set k=5 as default")
    local.knn.k  <- 5
  }else{
    local.knn.k  <- parameters$k
  }
  output.message <- paste0("KNN classifier with k=", local.knn.k)
  
  local.n.test <- nrow(test)
  local.levels <- levels(train$y)
  
  # Model fit
  local.fit <- FNN::knn(train = scale(train[,1:50]),
                        test  = scale(test[,1:50]),
                        cl    = train$y,
                        k     = local.knn.k)
  levels(local.fit) <- local.levels
  
  # Prediction
  # Prediction is performed in `knn` fitting
  local.pred <- local.fit
  
  # Evaluate the prediction accuracy
  res <- f_clas_perf(test_class = test$y, 
                     pred_class = local.pred)
  # Return result
  message(output.message)
  return(res)
}

message("'f_clas_knn' is imported")