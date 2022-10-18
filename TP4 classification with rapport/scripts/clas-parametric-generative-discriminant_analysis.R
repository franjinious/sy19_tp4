###########################################################################
# clas-parametric-generative-discriminant_analysis.R ----------------------
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

if(!require("MASS")) { # for QDA and LDA
  install.packages("MASS")
  library("MASS")
}

if(!require("naivebayes")) { # For naive bayes
  install.packages("naivebayes")
  library("naivebayes")
}

if(!require("nnet")) { # For naive bayes
  install.packages("nnet")
  library("nnet")
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

# Fit Model(s) to pre-generated train & test sets -------------------------
# Define parameter(s)
message("No parameter needed in QDL, LDA and Naive Bayes")

# Fit model
clas.qda.fit         <- qda        (y ~ ., data=clas.data.train)
clas.lda.fit         <- lda        (y ~ ., data=clas.data.train)
clas.naive_bayes.fit <- naive_bayes(y ~ ., data=clas.data.train)

# Prediction 
tmp.qda.pred         <- predict(clas.qda.fit        , newdata=clas.data.test)
tmp.lda.pred         <- predict(clas.lda.fit        , newdata=clas.data.test)
tmp.naive_bayes.pred <- list(class     = predict(clas.naive_bayes.fit, newdata=clas.data.test, type = "class"),
                             posterior = predict(clas.naive_bayes.fit, newdata=clas.data.test, type = "prob" ))

clas.qda.pred         <- tmp.qda.pred$class         
clas.lda.pred         <- tmp.lda.pred$class         
clas.naive_bayes.pred <- tmp.naive_bayes.pred$class 

# correct 'levels' of predicted classes
levels(clas.qda.pred)         <- clas.levels
levels(clas.lda.pred)         <- clas.levels
levels(clas.naive_bayes.pred) <- clas.levels

# Contingency matrix
clas.qda.perf         <- table(clas.data.test$y, clas.qda.pred         )
clas.lda.perf         <- table(clas.data.test$y, clas.lda.pred         )
clas.naive_bayes.perf <- table(clas.data.test$y, clas.naive_bayes.pred )

clas.qda.perf
clas.lda.perf
clas.naive_bayes.perf


clas.qda.perf.error_total         <- 1 - sum (diag(clas.qda.perf         )) / nrow(clas.data.test)
clas.lda.perf.error_total         <- 1 - sum (diag(clas.lda.perf         )) / nrow(clas.data.test)
clas.naive_bayes.perf.error_total <- 1 - sum (diag(clas.naive_bayes.perf )) / nrow(clas.data.test)

print(clas.qda.perf.error_total         , digits = 2)
print(clas.lda.perf.error_total         , digits = 2)
print(clas.naive_bayes.perf.error_total , digits = 2)

# Error within each class
clas.qda.perf.error_within_class         <- 1 - diag(clas.qda.perf         ) / rowSums(clas.qda.perf         )
clas.lda.perf.error_within_class         <- 1 - diag(clas.lda.perf         ) / rowSums(clas.lda.perf         )
clas.naive_bayes.perf.error_within_class <- 1 - diag(clas.naive_bayes.perf ) / rowSums(clas.naive_bayes.perf )

print(clas.qda.perf.error_within_class         , digits = 2)
print(clas.lda.perf.error_within_class         , digits = 2)
print(clas.naive_bayes.perf.error_within_class , digits = 2)

# { # Cleanup R env
#   rm(list = ls(pattern = ".qda."))
# Accuracy on Test data
# Error total
#   rm(list = ls(pattern = ".lda."))
#   rm(list = ls(pattern = ".naive_bayes."))
# }

# Integrate fit, pred and perf in to functions ----------------------------

# load performance evaludation function
source("scripts/f_clas_perf.R")
f_clas_perf

## QDA ******
source("scripts/f_clas_qda.R")
f_clas_qda
f_clas_qda(train = clas.data.train, test = clas.data.test)

clas.qda.perf
clas.qda.perf.error_total
clas.qda.perf.error_within_class

## LDA ******
source("scripts/f_clas_lda.R")
f_clas_lda(train = clas.data.train, test = clas.data.test)

clas.lda.perf
clas.lda.perf.error_total
clas.lda.perf.error_within_class

## Naive Bayes ******
source("scripts/f_clas_naive_bayes.R")
f_clas_naive_bayes(train = clas.data.train, test = clas.data.test)

clas.naive_bayes.perf
clas.naive_bayes.perf.error_total
clas.naive_bayes.perf.error_within_class


# Replicate
{# Repartition train & test
  clas.test.id <- createDataPartition(TPN1_a22_clas_app$y, p = 1/5, list = TRUE)
  clas.data.test  <- TPN1_a22_clas_app[ clas.test.id[[1]],]
  clas.data.train <- TPN1_a22_clas_app[-clas.test.id[[1]],]


f_clas_qda(         train = clas.data.train, test = clas.data.test )
f_clas_lda(         train = clas.data.train, test = clas.data.test )
f_clas_naive_bayes( train = clas.data.train, test = clas.data.test )


}

# Cross-validation --------------------------------------------------------

source("scripts/f_clas_cv.R")

# homemade Cross-validation
clas_qda.cv_repeat         <- f_clas_cv_repeat(f_classifier = f_clas_qda        , train = TPN1_a22_clas_app, k_folds = 5, repeats = 10)
clas_lda.cv_repeat         <- f_clas_cv_repeat(f_classifier = f_clas_lda        , train = TPN1_a22_clas_app, k_folds = 5, repeats = 10)
clas_naive_bayes.cv_repeat <- f_clas_cv_repeat(f_classifier = f_clas_naive_bayes, train = TPN1_a22_clas_app, k_folds = 5, repeats = 10)

clas_qda.cv_repeat[,1:4] %>% summary()
clas_lda.cv_repeat[,1:4] %>% summary()
clas_naive_bayes.cv_repeat[,1:4] %>% summary()

  clas_lda.cv_repeat[,1:4] %>% summarise()
clas_naive_bayes.cv_repeat[,1:4] %>% summarise()

# Cross-validation as function
source("scripts/f_clas_cv.R")





# ML ----------------------------------------------------------------------

# une seule realisation
?multinom
logit_multi_fit  <- nnet::multinom(y ~ ., data=clas.data.train)
logit_multi_pred <- predict(logit_multi_fit, newdata=clas.data.test)
logit_multi_perf <- table(clas.data.test$y, logit_multi_pred)
logit_multi_perf
sum(diag(logit_multi_perf))/ nrow(clas.data.test)
1-sum(diag(logit_multi_perf)) / nrow(clas.data.test) #erreur:0.4


source("scripts/f_clas_mulit_logistic.r")

{# Repartition train & test
  clas.test.id <- createDataPartition(TPN1_a22_clas_app$y, p = 1/5, list = TRUE)
  clas.data.test  <- TPN1_a22_clas_app[ clas.test.id[[1]],]
  clas.data.train <- TPN1_a22_clas_app[-clas.test.id[[1]],]
}
f_clas_mulit_logistic(train = clas.data.train, test = clas.data.test)
# Cross-validation --------------------------------------------------------
clas_mulit_logistic.cv_repeat <- f_clas_cv_repeat(f_classifier = f_clas_mulit_logistic, train = TPN1_a22_clas_app, k_folds = 5, repeats = 10)
clas_mulit_logistic.cv_repeat[,1:4] %>% summary()

require("glmnet")
lasso.mod =glmnet (clas.data.train[,1:50] ,clas.data.train$y,alpha =1, lambda =grid)

# Draft -------------------------------------------------------------------
dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(dat,aes(x=xx, colour = yy, fill = yy)) + 
  geom_density(alpha = 0.2)

# Clean up env ------------------------------------------------------------
rm(list = ls(pattern = "local."))
rm(list = ls(pattern = "tmp."))
rm(list = ls())

