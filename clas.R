
# CLASSFICATION -----------------------------------------------------------



# SETUP -------------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
reloadpck = function(reload = TRUE){
  if(reload){
    print(ipak(pckToLoad))
  }
}

pckToLoad = c('MASS'       , # QDA, LDQ
              'naivebayes' , # for naive QDA (a.k.a naive Bayes)
              'nnet'       , # Multinomial logistic regression
              'FNN'        , # Fast KNN algo
              # 'pROC'    , # ROC curve
              'tidyverse'  , # a collection of Rpack for data science
              'esquisse'   , # data visualisation 
              "GGally") 
reloadpck()



# END SETUP ---------------------------------------------------------------



# Import data -------------------------------------------------------------

clas.set <- read.csv("TPN1_a22_clas_app.txt", sep="")
str(clas.set) 
clas.set$y <- as.factor(clas.set$y) #change Y  to factor si non c'est int

# END Import data ---------------------------------------------------------


# Data exploration  -------------------------------------------------------


summary(clas.set)
length(clas.set)
NROW(clas.set)
n_clas <- dim(clas.set)[1]
barplot(table(clas.set$y))

tmp <- max(table(clas.set$y)/n_tot)
tmp #le taux de classes 2 ou 3
1-tmp 


# END data exploration --------------------------------------------------------




# Training & test set preparation -------------------------------------------------

n_tot <- dim(clas.set)[1]
n_app <- round(n_tot* 4/5)
n_tst <- n_tot - n_app

set.seed(19)
id_app <- sample(1:n_tot, n_app)

data.train <- clas.set[  id_app,]
data.test <- clas.set[- id_app,]



# KNN with an arbitrary k ---------------------------------------------


knn_k <- 10
knn_fit <- knn(train = scale(data.train[,1:50]),
               test  = scale(data.test[,1:50]),
               cl=data.train$y,
               k = knn_k,
               prob = TRUE)
knn_perf <- table(data.test$y, knn_fit)
knn_perf # matrice de confusion

cat("taux de succès:")
sum (diag(knn_perf)) / n_tst# taux de succès
cat("taux d'erreur:")
err.knn10 <- 1-sum (diag(knn_perf)) / n_tst # taux d'erreur





# KNN with K from 1 to Kmax -----------------------------------------------

knn_k_max <- 100

tmp <- sapply(1:knn_k_max, function(local_k){
  local_fit <- knn(train = scale(data.train[,1:50]),
                   test  = scale(data.test[,1:50]),
                   cl=data.train$y,
                   k = local_k,
                   prob = TRUE)
  local_perf <- table(data.test$y, local_fit)
  res <- sum (diag(local_perf)) / n_tst
  return(res)
})

err.knn_scale <- 1 - tmp
err.knn_scale
#erreur globale
plot(1:knn_k_max, err.knn_scale, 
     type='b', col='blue',
     ylim=range(err.knn_scale),
     xlab='k', ylab='Error rate', lty = 1, pch = 1)

#Error rate augmente en ~K=45

err.knn_scale_mean <- sum(err.knn_scale)/length(err.knn_scale)
cat("taux d'erreur moyen avec 100 différents K:")
err.knn_scale_mean #mauvaise erreurs en moyenne avec KNN


cat("taux d'erreur minimal avec 100 différents K:")
min(err.knn_scale)
cat("Ce taux d'erreur est minimal pour K=")
which.min(err.knn_scale)
#taux d'erreur minimal

 # ajoute un rouge pour groupe validation

# Parametric methods ------------------------------------------------------


# QDA ---------------------------------------------------------------------

fit.qda  <- qda(y ~ ., data=data.train)
pred.qda <- predict(fit.qda, newdata=data.test)
perf.qda <- table(data.test$y, pred.qda$class)
perf.qda
sum (diag(perf.qda)) / n_tst 
err.qda <- 1-sum (diag(perf.qda)) / n_tst
cat("Taux d'erreur avec QDA:")
err.qda

# KCV for QDA

K<-10
CV=0
CV2=0
folds=sample(1:K,n_tot,replace=TRUE)
for(k in (1:K)){
  class<-qda(y~.,data=clas.set[folds!=k,])
  pred<-predict(class,newdata=clas.set[folds==k,])
  conf <- table(clas.set[folds==k,]$y, pred$class)
  CV<-CV+1-sum(diag(conf))/nrow(clas.set[folds==k,])
#  class<-qda(y~.,data=clas.set[folds==k,])
#  pred<-predict(class,newdata=clas.set[folds==k,])
#  conf <- table(clas.set[folds==k,]$y, pred$class)
#  CV2<-CV2+1-sum(diag(conf))/nrow(clas.set[folds==k,])
}
CV<-CV/K
#CV2<-CV2/K



# LDA ---------------------------------------------------------------------


fit.lda  <- lda(y ~ ., data=data.train)
pred.lda <- predict(fit.lda, newdata=data.test)
perf.lda <- table(data.test$y, pred.lda$class)
perf.lda
fit.lda  <- lda(y ~ ., data=data.train)
pred.lda <- predict(fit.lda, newdata=data.test)
perf.lda <- table(data.test$y, pred.lda$class)
perf.lda
sum(diag(perf.lda)) / n_tst
err.lda <- 1-sum(diag(perf.lda)) / n_tst #erreur 0.34
cat("taux d'erreur avec LDA:")
err.lda






# Naive bayes -------------------------------------------------------------

fit.naiveqda  <- naive_bayes(y ~ ., data=data.train)
pred.naiveqda <- predict(fit.naiveqda, newdata=data.test[1:50])
perf.naiveqda <- table(data.test$y, pred.naiveqda)
perf.naiveqda
sum(diag(perf.naiveqda)) / n_tst
err.naiveqda <- 1-sum(diag(perf.naiveqda)) / n_tst
cat("taux d'erreur avec LDA:")
err.naiveqda


# Naive bayes AFTER lasso coefficient selection -------------------------

library(glmnet)
x<-model.matrix(y~.,class)
y<-class$y
x.train <- x[id_app,]
y.train <- y[id_app]
x.test <- x[-id_app,]
y.test <- y[-id_app]

cv.out.lasso <- cv.glmnet(x.train, y.train, alpha = 1)
# library(coefplot)
coefs_extracted<-extract.coef(cv.out.lasso)
coefs<-row.names(coefs_extracted)[-1]
plot(cv.out.lasso)
fit.lasso <- glmnet(x.train, y.train, lambda = cv.out.lasso$lambda.min, alpha = 1)
lasso.predict <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = x.test)
mse.lasso <- mean((lasso.predict - y.test) ^ 2)#178

fit.naiveqda  <- naive_bayes(y ~ X3 + X5 + X6 + X16 + X26 + X40 + X44 + X47, data=data.train)
pred.naiveqda <- predict(fit.naiveqda, newdata=data.test[1:50])
perf.naiveqda <- table(data.test$y, pred.naiveqda)
perf.naiveqda
sum(diag(perf.naiveqda)) / n_tst
err.naiveqda <- 1-sum(diag(perf.naiveqda)) / n_tst
cat("taux d'erreur avec LDA:")
err.naiveqda
car("L'erreur est encore plus grande et la sélection de coefficient par lasso n'a pas fonctionné.")





# Multinomial logistic regression -----------------------------------------
#Ici, nos données ont les classes c > 2, donc on a utilisé la méthode "Multinomial logistic regression"


#lasso + selection de variable 
#https://fr.wikipedia.org/wiki/R%C3%A9gression_logistique

fit.logistic_multi  <- multinom(y ~ ., data=data.train)
pred.logistic_multi <- predict(fit.logistic_multi, newdata=data.test)
perf.logistic_multi <- table(data.test$y, pred.logistic_multi)
perf.logistic_multi
sum(diag(perf.logistic_multi)) / n_tst
err.logistic_multi <- 1-sum(diag(perf.logistic_multi)) / n_tst 
err.logistic_multi

# Logistic regression AFTER lasso coefficient selection -------------------------

fit.logistic_multi  <- multinom(y ~ X3 + X5 + X6 + X16 + X26 + X40 + X44 + X47, data=data.train)
pred.logistic_multi <- predict(fit.logistic_multi, newdata=data.test)
perf.logistic_multi <- table(data.test$y, pred.logistic_multi)
perf.logistic_multi
sum(diag(perf.logistic_multi)) / n_tst
err.logistic_multi_lasso <- 1-sum(diag(perf.logistic_multi)) / n_tst 


# ROC CURVE -------------------------------------------------------

install.packages("pROC")
library(pROC)

roc.lda<-roc(data.test$y,as.vector(pred.lda$posterior[,1]))
plot(roc.lda)

roc.qda<-roc(data.test$y,as.vector(pred.qda$posterior[,1]))
plot(roc.qda, col='red', add=TRUE)

roc.naive<-roc(data.test$y,pred.naiveqda[,1])
plot(roc.naive, col='green', add=TRUE)

# not feasible with logistic regression

#model selection


# Feature extraction  -----------------------------------------------------


#PCA (unsupervised)
X <- clas.set[1:50]
X<-scale(X)
pca<-princomp(X)
Z<-pca$scores
lambda<-pca$sdev^2
plot(cumsum(lambda)/sum(lambda),type="l",xlab="q",ylab="proportion of explained variance")
#variance augmenter tres lentement en funciton de q

pairs(Z[,1:5],col=clas.set[,51],pch=clas.set[,51])
pairs(Z[,6:10],col=clas.set[,51],pch=clas.set[,51])
str(clas.set[,51])

#FDA (supervised)

lda.fda<-lda(y~.,data = clas.set)
U<-lda.fda$scaling
X<-as.matrix(clas.set[,1:50])
Z<-X%*%U


plot(Z[,1],Z[,2],pch=clas.set$y,col=clas.set$y,xlab='Z1',ylab='Z2')


