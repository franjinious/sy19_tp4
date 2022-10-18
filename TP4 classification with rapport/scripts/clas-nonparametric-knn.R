###########################################################################
# clas-nonparametric-knn.R --------------------------------------------------------------
###########################################################################
# Set up ------------------------------------------------------------------

# Set working directory
if(!require(here)) install.packages("here")
if(require(here)) {
  setwd(here::here())
}else{
  warning('Make sure the package "here" is installed')
}

# Load r packages (Install if missing)
if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}

if(!require("caret")) {
  install.packages("caret")
  library("caret")
}


if(!require("FNN")) {
  install.packages("FNN")
  library("FNN")
}

# Load Data with basic statistics -----------------------------------------

source("scripts/clas-general-import_data.R")

# basic statistics
clas.levels <- levels(TPN1_a22_clas_app$y)

# Data preparation : Partitioning raw data to train & test ----------------
# create train & test
set.seed(69)
clas.test.id <- createDataPartition(TPN1_a22_clas_app$y,
                                     p = 1/5,
                                     list = TRUE)

# clas.train.id$Resample1 is the 1st component of clas.train.id
# we can also call clas.train.id[[1]] to call this component

clas.data.test  <- TPN1_a22_clas_app[ clas.test.id[[1]],]
clas.data.train <- TPN1_a22_clas_app[-clas.test.id[[1]],]

rm(clas.test.id)

# KNN with an arbitrary k -------------------------------------------------
# define parameter(s)
clas.knn.k <- 10

# Fit model
clas.knn.fit <- FNN::knn(train = scale(clas.data.train[,1:50]),
                         test  = scale(clas.data.test [,1:50]),
                         cl    = clas.data.train$y,
                         k     = clas.knn.k)

# Prediction 
# For KNN, the prediction is performed in `knn` function

# correct 'levels' of predicted classes
levels(clas.knn.fit) <- clas.levels

# Contingency matrix
clas.knn.perf <- table(clas.data.test$y, clas.knn.fit)
clas.knn.perf

# Evaluate the prediction accuracy (on test data)
# Error total
clas.knn.error_total   <- 1 - sum (diag(clas.knn.perf)) / nrow(clas.data.test)
print(clas.knn.error_total, digits = 2)

# Error within each class
clas.knn.error_within_class <- 1 - diag(clas.knn.perf) / rowSums(clas.knn.perf)
clas.knn.error_within_class

{# clean up r env
  rm(clas.levels)
  rm(clas.knn.k)
  rm(clas.knn.fit)
  rm(clas.knn.perf)
  rm(clas.knn.error_total)
  rm(clas.knn.error_within_class)
}

# Integrate fit, pred and perf in to functions ----------------------------

source("scripts/f_clas_perf.R")
source("scripts/f_clas_knn.R")

# Execute functions and compare with previous results ****
# run with an arbitrary model parameter
clas.knn.k <- 10
f_clas_knn(train = clas.data.train, 
           test  = clas.data.test, 
           parameters = list(k = clas.knn.k))

clas.knn.perf
clas.knn.error_total
clas.knn.error_within_class

# Replicate
{# Repartition train & test
  clas.test.id <- createDataPartition(TPN1_a22_clas_app$y, p = 1/5, list = TRUE)
  clas.data.test  <- TPN1_a22_clas_app[ clas.test.id[[1]],]
  clas.data.train <- TPN1_a22_clas_app[-clas.test.id[[1]],]


f_clas_knn(train = clas.data.train, 
           test  = clas.data.test, 
           parameters = list(k = clas.knn.k))

}

# KNN with a range of Ks --------------------------------------------------

# init a matrix (data.frame) to store the result
clas.knn.k_range <- 1:50
clas.knn.n_parameters <- 1

clas.test.errors <- matrix(data = NA,
                           ncol = 4 + clas.knn.n_parameters,
                           nrow = length(clas.knn.k_range))

clas.test.errors <- as.data.frame(clas.test.errors)
colnames(clas.test.errors) <- c('Err.tot', 'Err.1', 'Err.2', 'Err.3', 'param.k')

# loop : fit model to different model parameter(s)
for(i in 1:length(clas.knn.k_range)){
  
  tmp.param <- list(k = clas.knn.k_range[i])
  tmp.fit <- f_clas_knn(train = clas.data.train, 
                        test  = clas.data.test, 
                        parameters = tmp.param)
  
  clas.test.errors[i, ] <- c(tmp.fit$test.error.total,
                             tmp.fit$test.error.within_class,
                             unlist(tmp.param))
}

# Analyse Result

head(clas.test.errors)

ggplot(data = clas.test.errors, 
       mapping = aes(x = param.k)) +
  geom_line(aes(y = Err.tot, color = "Test Err (Total)")) + 
  geom_line(aes(y = Err.1, color = "Test Err in class 1"), linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Err.2, color = "Test Err in class 2"), linetype = "dashed", alpha = 0.8) +
  geom_line(aes(y = Err.3, color = "Test Err in class 3"), linetype = "dashed", alpha = 0.8)



#Result

clas.test.errors[which.min(clas.test.errors$Err.tot),]





{# clean up r env
  rm(clas.knn.n_parameters)
  rm(clas.knn.k_range)
  rm(clas.test.errors)
  rm(i)
  rm(list = ls(pattern = "tmp."))
  rm(clas.data.test)
  rm(clas.data.train)
  rm(clas.test.id)
}

# Cross-validation --------------------------------------------------------
#import function
source("scripts/f_clas_knn.R")
source("scripts/f_clas_cv.R")

f_clas_cv_repeat(f_classifier = f_clas_knn, train = TPN1_a22_clas_app, k_folds = 5, parameters = list(k = 5), repeats = 10)


tmp.k_range <- 1:100

tmp.res.list <- list()
for (i in 1:length(tmp.k_range)) {
  tmp.res.list[[i]] <- f_clas_cv_repeat(f_classifier = f_clas_knn, train = TPN1_a22_clas_app, k_folds = 5, parameters = list(k = tmp.k_range[i]), repeats = 10)
  tmp.res.list[[i]]$k <- tmp.k_range[i]
}

tmp.res.df <- do.call(rbind, tmp.res.list)

tmp <- tmp.res.df %>% 
  group_by(k) %>% 
  summarise(Err.tot.avg = mean(Err.tot),
            Err.tot.sd = sd(Err.tot), 
            Err.1.avg = mean(Err.1),
            Err.1.sd = sd(Err.1), 
            Err.2.avg = mean(Err.2),
            Err.2.sd = sd(Err.2), 
            Err.3.avg = mean(Err.3),
            Err.3.sd = sd(Err.3)) 

tmp %>% filter(Err.tot.avg == min(Err.tot.avg))

p <- tmp.res.df %>% 
  select(Err.tot, Err.1, Err.2, Err.3, k) %>% 
  filter(k <= 150) %>%
  gather(Err.tot, Err.1, Err.2, Err.3, key = "Err_type", value = "Err") %>% 
  ggplot(aes(x = as.factor(k), y = Err, fill=Err_type))

p + geom_boxplot() + 
  facet_grid(. ~ Err_type) +
  # facet_grid(Err_type ~ .) +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 3))


# Homemade Cross-validation
set.seed(69)
{# repeat CV and observe !
  clas.cv.k_folds <- 5
  tmp.cv.test.fold_id <- createFolds(y = TPN1_a22_clas_app$y, 
                                     k = clas.cv.k_folds,
                                     list = FALSE)
  
  # init a matrix (data.frame) to store the result
  tmp.k_range <- 1:100
  tmp.grid <- expand.grid(param.k = tmp.k_range, 
                          test.fold_id = 1:clas.cv.k_folds)
  
  tmp.errors <- matrix(data = NA,
                       ncol = 4,
                       nrow = nrow(tmp.grid))
  tmp.errors <- as.data.frame(tmp.errors)
  colnames(tmp.errors) [1:4] <- c('Err.tot', 'Err.1', 'Err.2', 'Err.3')
  clas.cv.test.errors <- cbind(tmp.errors, tmp.grid)
  
  # loop : fit model to different model parameter(s) in CV
  for(i in 1:nrow(clas.cv.test.errors)){
    tmp.fold_id <-          clas.cv.test.errors$test.fold_id[i]
    tmp.param   <- list(k = clas.cv.test.errors$param.k[i])
    
    tmp.data.test  <- TPN1_a22_clas_app[tmp.cv.test.fold_id == tmp.fold_id,]
    tmp.data.train <- TPN1_a22_clas_app[tmp.cv.test.fold_id != tmp.fold_id,]
    
    # loop : fit model to different model parameter(s)
    tmp.fit <- f_clas_knn(train = tmp.data.train,
                          test  = tmp.data.test,
                          parameters = tmp.param)
      
    clas.cv.test.errors[i, 1:4] <- c(tmp.fit$test.error.total,
                                     tmp.fit$test.error.within_class)
  }
  
  # Result
  head(clas.cv.test.errors)
  
  # best parameter k : 
  clas.cv.test.errors %>% 
    group_by(param.k) %>% 
    summarise(avg.Err.tot = mean(Err.tot)) %>% 
    filter(avg.Err.tot == min(avg.Err.tot))
}

clas.cv.test.errors %>% 
  ggplot(mapping = aes(x = param.k, group = test.fold_id)) +
  xlab("#Neighbours") + ylab("Error rate") +
  geom_line(aes(y = Err.tot, color = "Test Err (Total)")) + 
  geom_line(aes(y = Err.1  , color = "Test Err in class 1"), linetype = "dashed", alpha = 0.5) +
  geom_line(aes(y = Err.2  , color = "Test Err in class 2"), linetype = "dashed", alpha = 0.5) +
  geom_line(aes(y = Err.3  , color = "Test Err in class 3"), linetype = "dashed", alpha = 0.5) +
  theme_light()

clas.cv.test.errors %>% 
  group_by(param.k) %>% 
  summarise(avg.Err.tot = mean(Err.tot),
            avg.Err.1   = mean(Err.1),
            avg.Err.2   = mean(Err.2),
            avg.Err.3   = mean(Err.3)) %>% 
  ggplot(mapping = aes(x = param.k)) +
  ggtitle(paste0("Plot of Average error in ", clas.cv.k_folds, "-fold CV")) +
  xlab("#Neighbours") + ylab("Avg Error rate (Cross-Validation)") +
  geom_line(aes(y = avg.Err.tot , color = "Test Err (Total)")) + 
  geom_line(aes(y = avg.Err.1   , color = "Test Err in class 1"), linetype = "dashed", alpha = 1) +
  geom_line(aes(y = avg.Err.2   , color = "Test Err in class 2"), linetype = "dashed", alpha = 1) +
  geom_line(aes(y = avg.Err.3   , color = "Test Err in class 3"), linetype = "dashed", alpha = 1) +
  theme_light()


# Caret -------------------------------------------------------------------

clas.fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 20)

set.seed(825)
tmp.knn.fit <- train(y ~ ., data = TPN1_a22_clas_app, 
                     method = "knn", preProcess = "scale",
                     trControl = clas.fitControl,
                     tuneGrid = data.frame(k = 1:300))

trellis.par.set(caretTheme())
plot(tmp.knn.fit, ylim = c(0.4,0.70))

# Draft -------------------------------------------------------------------
#PCA (unsupervised)


X <- TPN1_a22_clas_app[1:50]
X<-scale(X)
pca<-princomp(X)
Z<-pca$scores
lambda<-pca$sdev^2
plot(cumsum(lambda)/sum(lambda),type="l",xlab="q",ylab="proportion of explained variance")
#variance augmenter tres lentement en funciton de q

pairs(Z[,1:3], col=TPN1_a22_clas_app[,51], pch=TPN1_a22_clas_app[,51])
pairs(Z[,6:10],col=clas.set[,51],pch=clas.set[,51])
str(clas.set[,51])
pairs(Z[,1:3],col=clas.set[,1],pch=clas.set[,1])


#FDA (supervised)

lda.fda<-lda(y~.,data = TPN1_a22_clas_app)
U<-lda.fda$scaling
X<-as.matrix(TPN1_a22_clas_app[,1:50])
Z<-X%*%U


plot(Z[,1],Z[,2],pch=TPN1_a22_clas_app$y,col=TPN1_a22_clas_app$y,xlab='Z1',ylab='Z2')
# Clean up env ------------------------------------------------------------
# rm(list = ls(pattern = "tmp."))
# rm(list = ls())
