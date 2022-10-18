f_clas_cv <- function(f_classifier, train, k_folds = 5, parameters = NULL){
  # init cv.errors data.frame
  local.cv.errors <- matrix(data = NA,
                            ncol = 4,
                            nrow = k_folds)
  local.cv.errors <- as.data.frame(local.cv.errors)
  colnames(local.cv.errors) [1:4] <- c('Err.tot', 'Err.1', 'Err.2', 'Err.3')
  local.cv.errors$fold_id = 1:k_folds
  
  # partition data into k folds (Stratified sampling according to y)
  local.cv.test.fold_ids <- createFolds(y = train$y,
                                        k = k_folds,
                                        list = FALSE)
  for (i in 1:k_folds) {
    # use fold_i as test 
    local.errors <- f_classifier(train = train[ local.cv.test.fold_ids != i, ], 
                                 test  = train[ local.cv.test.fold_ids == i, ],
                                 parameters = parameters)
    
    local.cv.errors[i, 1:4] <- c(local.errors$test.error.total, local.errors$test.error.within_class)
  }
  return(local.cv.errors)
}


f_clas_cv_repeat <- function(f_classifier, train, k_folds = 5, 
                             repeats = 4, 
                             parameters = NULL,
                             repeat_id.as.cv.seed = FALSE,
                             cv.seed.shift = 0){
  
  local.res.list <- list()
  for(i in 1:repeats){
    message(paste0("Repeat id = ", i, " ==============================="))
    if(repeat_id.as.cv.seed) set.seed(i + cv.seed.shift)
    
    local.res <- f_clas_cv(f_classifier, train, k_folds, parameters)
    local.res$repeat_id = rep(i, k_folds)
    local.res.list[[i]] <- local.res
  }
  res <- do.call(rbind, local.res.list)
  return(res)
}

message("'f_clas_cv' and 'f_clas_cv_repeat' are imported")


