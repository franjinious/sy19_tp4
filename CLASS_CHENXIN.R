
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

TPN1_a22_clas_app <- read.csv("TPN1_a22_clas_app.txt", sep="")
str(TPN1_a22_clas_app) 
TPN1_a22_clas_app$y   <- as.factor(TPN1_a22_clas_app$y) #change Y  to factor si non c'est int

# END Import data ---------------------------------------------------------


# Data exploration  -------------------------------------------------------


summary(TPN1_a22_clas_app)
length(TPN1_a22_clas_app)
NROW(TPN1_a22_clas_app)
n_tot <- dim(TPN1_a22_clas_app)[1]
barplot(table(TPN1_a22_clas_app$y))

tmp <- max(table(TPN1_a22_clas_app$y)/n_tot)
tmp #le taux de success minimum
1-tmp #le taux d'erreur maximum


# END data exploration --------------------------------------------------------




# Training & test set prepration -------------------------------------------------

n_tot <- dim(TPN1_a22_clas_app)[1]
n_app <- round(n_tot* 4/5)
n_tst <- n_tot - n_app

set.seed(19)
id_app <- sample(1:n_tot, n_app)

data_app <- TPN1_a22_clas_app[  id_app,]
data_tst <- TPN1_a22_clas_app[- id_app,]



# KNN with an arbitrary k ---------------------------------------------


knn_k <- 10
knn_fit <- knn(train = scale(data_app[,1:50]),
               test  = scale(data_tst[,1:50]),
               cl=data_app$y,
               k = knn_k,
               prob = TRUE)
knn_perf <- table(data_tst$y, knn_fit)
knn_perf

sum (diag(knn_perf)) / n_tst #taux d'success ;0.58
1-sum (diag(knn_perf)) / n_tst #taux d'erreur:0.42





# KNN with K from 1 to Kmax -----------------------------------------------

knn_k_max <- 100
tmp <- sapply(1:knn_k_max, function(local_k){
  local_fit <- knn(train = scale(data_app[,1:50]),
                   test  = scale(data_tst[,1:50]),
                   cl=data_app$y,
                   k = local_k,
                   prob = TRUE)
  local_perf <- table(data_tst$y, local_fit)
  res <- sum (diag(local_perf)) / n_tst
  return(res)
})

knn_MSE_scale <- 1 - tmp
knn_MSE_scale
#erreur global
plot(1:knn_k_max, knn_MSE_scale, 
     type='b', col='blue',
     ylim=range(knn_MSE_scale),
     xlab='k', ylab='MSE', lty = 1, pch = 1)


#MSE augumante en ~K=45

which.min(knn_MSE_scale)
min(knn_MSE_scale)
#taux d'erreur minmial 0.38 with k =31


 # ajoute un rough pour groupe validation

# Parametric methods ------------------------------------------------------


# QDA ---------------------------------------------------------------------

qda_fit  <- qda(y ~ ., data=data_app)
qda_pred <- predict(qda_fit, newdata=data_tst)
qda_perf <- table(data_tst$y, qda_pred$class)
qda_perf
sum (diag(qda_perf)) / n_tst 
1-sum (diag(qda_perf)) / n_tst #erreur:0.35


# LDA ---------------------------------------------------------------------


lda_fit  <- lda(y ~ ., data=data_app)
lda_pred <- predict(lda_fit, newdata=data_tst)
lda_perf <- table(data_tst$y, lda_pred$class)
lda_perf
lda_fit  <- lda(y ~ ., data=data_app)
lda_pred <- predict(lda_fit, newdata=data_tst)
lda_perf <- table(data_tst$y, lda_pred$class)
lda_perf
sum(diag(lda_perf)) / n_tst
1-sum(diag(lda_perf)) / n_tst #erreur 0.34



# Naive bayes -------------------------------------------------------------

qda_naive_fit  <- naive_bayes(y ~ ., data=data_app)
qda_naive_pred <- predict(qda_naive_fit, newdata=data_tst[1:50])
qda_naive_perf <- table(data_tst$y, qda_naive_pred)
qda_naive_perf
sum(diag(qda_naive_perf)) / n_tst
1-sum(diag(qda_naive_perf)) / n_tst # errer:0.31



# Multinomial logistic regression -----------------------------------------
#Ici, nos données ont les classes c > 2, donc on a utilisé la méthode "Multinomial logistic regression"


#lasso + selection de variable 
#https://fr.wikipedia.org/wiki/R%C3%A9gression_logistique

logit_multi_fit  <- multinom(y ~ ., data=data_app)
logit_multi_pred <- predict(logit_multi_fit, newdata=data_tst)
logit_multi_perf <- table(data_tst$y, logit_multi_pred)
logit_multi_perf
sum(diag(logit_multi_perf)) / n_tst
1-sum(diag(logit_multi_perf)) / n_tst #erreur:0.4




#model selection


# Feature extraction  -----------------------------------------------------


#PCA (unsupervised)
X <- TPN1_a22_clas_app[1:50]
X<-scale(X)
pca<-princomp(X)
Z<-pca$scores
lambda<-pca$sdev^2
plot(cumsum(lambda)/sum(lambda),type="l",xlab="q",ylab="proportion of explained variance")
#variance augmenter tres lentement en funciton de q

pairs(Z[,1:5],col=TPN1_a22_clas_app[,51],pch=TPN1_a22_clas_app[,51])
pairs(Z[,6:10],col=TPN1_a22_clas_app[,51],pch=TPN1_a22_clas_app[,51])
str(TPN1_a22_clas_app[,51])

#FDA (supervised)


lda.fda<-lda(TPN1_a22_clas_app$y~.,data = TPN1_a22_clas_app)
U<-lda.fda$scaling
X<-as.matrix(TPN1_a22_clas_app[,1:50])
Z<-X%*%U


plot(Z[,1],Z[,2],pch=TPN1_a22_clas_app$y,col=TPN1_a22_clas_app$y,xlab='Z1',ylab='Z2')
