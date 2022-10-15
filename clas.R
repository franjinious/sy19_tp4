
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
              'pROC'    , # ROC curve
              'tidyverse'  , # a collection of Rpack for data science
              'esquisse'   , # data visualisation 
              "GGally"     , 
              "glmnet"     , # lasso & ridge
              "coefplot"   , # get lasso & ridge coef more easily
              "leaps"      , # subset selection
              "dplyr"      , # subset selection
              )
reloadpck()



# END SETUP ---------------------------------------------------------------



# Import data -------------------------------------------------------------

clas.set <- read.csv("TPN1_a22_clas_app.txt", sep="")
str(clas.set) 

# END Import data ---------------------------------------------------------


# Data exploration  -------------------------------------------------------


summary(clas.set)
length(clas.set)
NROW(clas.set)
n_clas <- dim(clas.set)[1]
barplot(table(clas.set$y))

tmp <- max(table(clas.set$y)/n_clas)
tmp #le taux de classes 2 ou 3
1-tmp 


# END data exploration --------------------------------------------------------




# Training & test set preparation -------------------------------------------------

n_clas <- dim(clas.set)[1]
train_percentage <- 4/5
n_app <- round(n_clas* train_percentage)
n_tst <- n_clas - n_app

set.seed(19)
id_train <- sample(1:n_clas, n_app)

data.train <- clas.set[  id_train,]
data.test <- clas.set[- id_train,]
y.test <- clas.set[-id_train, c(51)]
y.train <- clas.set[id_train, c(51)]



# Sélection de modèles ------------------------------------------------

summary(lm(y~., data=data.train))
formula <- c(y~.)
summary(lm(y ~ X26+X44+X47+X40+X24+X19+X16+X5, data=data.train))
cat("On peut déjà sélectionner cette formule avec ces coefficient car c'est le R² ajusté qui est le plus haut avec une RSS plus faible que le modèle avec tous les coefficients.")
formula <- append(formula, y~X26+X44+X47+X40+X24+X19+X16+X5)


#Subset selection
#Vu qu'il existe trop de variables, nous pourrions pas utiliser la m??thode best subset selection

#forward selection
library(leaps)
library(dplyr)
reg.selection.forward <- regsubsets(y~., data = data.train, method = "forward", nbest = 1, nvmax = 100)
summary_forward <- summary(reg.selection.forward)
plot(reg.selection.forward, scale = "adjr2")#Regarder brièvement la plus grande adjusted R Square

rss<-data.frame(summary_forward$outmat, RSS=summary_forward$rss)
rsquare_max_forward <- summary_forward$outmat[which.max(summary_forward$adjr2),]#La ligne avec la plus grande adjr2
rsquare_max_forward[rsquare_max_forward == '*'] <- as.numeric(1)
rsquare_max_forward[rsquare_max_forward == ' '] <- as.numeric(0)
rsquare_max_forward <- as.numeric(rsquare_max_forward)#Le masque pour s??lectionner les variables
reg.subset.forward <- clas.set[c(rsquare_max_forward==1)]


n.subset.forward <- nrow(reg.subset.forward)
set.seed(69)
n.subset.forward.train <- as.integer(train_percentage * n.subset.forward)
n.subset.forward.sample <- sample(n.subset.forward, n.subset.forward.train)
reg.subset.forward.train <- reg.subset.forward[n.subset.forward.sample,]
reg.subset.forward.test <- reg.subset.forward[-n.subset.forward.sample,]
reg.subset.forward.lm <- lm(formula = y~., data = reg.subset.forward.train)
reg.subset.forward.lm.predict <- predict(reg.subset.forward.lm, newdata = reg.subset.forward.test)
reg.subset.forward.mse <- mean((reg.subset.forward.lm.predict - reg.subset.forward.test$y) ^ 2)#188.44
reg.subset.forward.err <- rstandard(reg.subset.forward.lm)
plot(reg.subset.forward.train$y, reg.subset.forward.err)
abline(0, 0)

#backward selection
reg.selection.backward <- regsubsets(y~., data = data.train, method = "backward", nbest = 1, nvmax = 100)
summary_backward <- summary(reg.selection.backward)
plot(reg.selection.backward, scale = "adjr2")
#rss<-data.frame(summary_forward$outmat, RSS=summary_forward$rss)
rsquare_max_backward <- summary_backward$outmat[which.max(summary_backward$adjr2),]
rsquare_max_backward[rsquare_max_backward == '*'] <- as.numeric(1)
rsquare_max_backward[rsquare_max_backward == ' '] <- as.numeric(0)
rsquare_max_backward <- as.numeric(rsquare_max_backward)
reg.subset.backward <- clas.set[c(rsquare_max_backward==1)]

n.subset.backward <- nrow(reg.subset.backward)
set.seed(69)
n.subset.backward.train <- as.integer(train_percentage * n.subset.backward)
n.subset.backward.sample <- sample(n.subset.backward, n.subset.backward.train)
reg.subset.backward.train <- reg.subset.backward[n.subset.backward.sample,]
reg.subset.backward.test <- reg.subset.backward[-n.subset.backward.sample,]
reg.subset.backward.lm <- lm(formula = y~., data = reg.subset.backward.train)
reg.subset.backward.lm.predict <- predict(reg.subset.backward.lm, newdata = reg.subset.backward.test)
reg.subset.backward.mse <- mean((reg.subset.backward.lm.predict - reg.subset.backward.test$y) ^ 2)#186.92
reg.subset.backward.err <- rstandard(reg.subset.backward.lm)
plot(reg.subset.backward.train$y, reg.subset.backward.err)
abline(0, 0)

res.sum <- summary(reg.selection.backward)
Adj.R2 = rss<-data.frame(res.sum$outmat, Adj.R2=res.sum$adjr2)
BIC = rss<-data.frame(res.sum$outmat, BIC=res.sum$bic)


cat("On obtient une MSE égale e une formule égale dans les deux types de sélections (~0.5097844)")



#k-cross validation sur backward selection
#avec
FormulaBackward <- c(y~.-X2, 
                     y~.-X2-X4, 
                     y~.-X2-X4-X7, 
                     y~.-X2-X4-X7-X8,
                     y~.-X2-X4-X7-X8-X9,
                     y~.-X2-X4-X7-X8-X9-X10,
                     y~.-X2-X4-X7-X8-X9-X10-X11,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13, 
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14, 
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15, 
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20, 
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21, 
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43-X45,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43-X45-X46,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43-X45-X46-X48,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43-X45-X46-X48-X49,
                     y~.-X2-X4-X7-X8-X9-X10-X11-X12-X13-X14-X15-X18-X20-X21-X25-X27-X29-X31-X32-X34-X35-X36-X37-X39-X43-X45-X46-X48-X49-X50
)

K <- 10
fold <- sample(K, n_train, replace = TRUE)
CV <- rep(0, 10)
for (i in (1:10)){
  for (k in (1:K)){
    reg.cross<-lm(FormulaBackward[[i]],data=clas.set[fold!=k,])
    pred.cross <- predict(reg.cross, newdata=clas.set[fold == k,])
    CV[i]<-CV[i]+ sum((clas.set$y[fold==k]-pred.cross)^2)
  }
  CV[i]<-CV[i] / n_reg
}
CV.min = min(CV)# 0.52

formula <- append(formula, y ~ X1 + X3 + X5 + X6 + X16 + X17 + X19 + X22 + X23 + X24 + X26 + X28 + X30 + X33 + X38 + X40 + X41 + X42 + X44 + X47)

clas.set$y <- as.factor(clas.set$y) #change Y  to factor pour les classifications


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
folds=sample(1:K,n_clas,replace=TRUE)
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

data.train$y <- factor(data.train$y)

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
clas.set$y <- as.numeric(clas.set$y)
x<-model.matrix(y~.,clas.set)
y<clas.set$y
x.train <- x[id_train,]
y.train <- y[id_train]
x.test <- x[-id_train,]
y.test <- y[-id_train]

cv.out.lasso <- cv.glmnet(x.train, y.train, alpha = 1)
library(coefplot)
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
cat("L'erreur est encore plus grande et la sélection de coefficient par lasso n'a pas fonctionné.")





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


