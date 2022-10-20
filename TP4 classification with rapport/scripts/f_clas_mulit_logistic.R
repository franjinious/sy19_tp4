if(!exists("f_clas_mulit_logistic")) source("scripts/f_clas_perf.R")

f_clas_mulit_logistic <- function(train, test, parameters = NULL){
  require("nnet")
  
  # Import parameters
  if(!is.null(parameters)) {
    warning("`parameters` argument is not in use, as 'Multinomial logistic regression' needs no parameter")
  }
  output.message <- "mulit_logistic classifier"
  
  local.n.test <- nrow(test)
  local.levels <- levels(train$y)
  
  # Model fit
  local.fit <- nnet::multinom(y ~ ., data = train)
  
  # Prediction
  local.pred <- predict(local.fit , newdata = test[ ,!(names(test) == "y")])
  levels(local.pred) <- local.levels
  
  # Evaluate the prediction accuracy
  res <- f_clas_perf(test_class = test$y, 
                     pred_class = local.pred)
  
  # Return result
  message(output.message)
  return(res)
}

message("'f_clas_mulit_logistic' is imported")