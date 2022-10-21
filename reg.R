library(MASS)
reg.set <- read.table('TPN1_a22_reg_app.txt', header = TRUE)

#plot(reg.set[,1:10], reg.set$y)

#Séparation des partie train et test
train.percentage <- 2/3
n_reg <- nrow(reg.set)
n_train <- as.integer(n_reg * train.percentage)
n_test <- n_reg - n_train
set.seed(69)
id_train <- sample(n_reg, n_train)
data.train <- reg.set[id_train,]
data.test <- reg.set[-id_train,]
y.test <- reg.set[-id_train, c(101)]
y.train <- reg.set[id_train, c(101)]

#Regarder la plage des données
boxplot(as.data.frame(reg.set[1:100]))


#pca
library(pls)
pcr_model <- pcr(y~., data = data.test, validation = "CV")
validationplot(pcr_model, val.type="MSEP")#D'apr??s le graphe on constate qu'il faut toutes les variables pour obtenir le meilleur mod??le


#modele lineare#
reg.set.lm <- lm(formula = y ~., data = data.train)
summary(reg.set.lm)#On a d??j?? une p-value qui est assez petite en une R-Square assez grande
res_std <- rstandard(reg.set.lm)
plot(x = y.train, y = res_std)#Les r??sidus stadards parraissent pas mal
abline(0, 0)
pre.lm <- predict(reg.set.lm, newdata = data.test)
mse.lm <- mean((pre.lm - y.test) ^ 2)#200.176
#On refait la regression avec les variables significatives
#x.app.lm <- data.train[c(6, 11, 12, 15, 17,22, 23, 25, 27, 32, 33, 35, 37, 39, 46, 47, 48, 49, 52, 54, 56, 59, 60, 63, 68, 70, 72, 74, 79, 83, 84, 87, 88, 89, 90, 91, 96), ] 
#data.test.lm <- data.test[c(6, 11, 12, 15, 17, 22, 23, 25, 27, 32, 33, 35, 37, 39, 46, 47, 48, 49, 52, 54, 56, 59, 60, 63, 68, 70, 72, 74, 79, 83, 84, 87, 88, 89, 90, 91, 96), ]
#reg.set.lm.revise <- lm(formula = y~. , data = x.app.lm)
reg.set.lm.revise <- lm(formula = y~X6+X11+X12+X15+X17+X22+X23+X25+X27+X32+X33+X35+X37+X39+X46+X47+X48+X49+X52+X54+X56+X59+X60+X63+X68+X70+X72+X74+X79+X83+X84+X87+X88+X89+X90+X91+X96
                        , data = data.train)
#pre.lm.revise <- predict(reg.set.lm.revise, newdata = data.test.lm)
pre.lm.revise <- predict(reg.set.lm.revise, newdata = data.test)
mse.lm1 <- mean((pre.lm.revise - y.test) ^ 2)#200.176



#k plus proches voisins
#On utilise les variables significatives dans le modèle lineaire
library(FNN)
x.app_k <- data.train[c(6, 11, 12, 15, 17, 22, 23, 25, 27, 32, 33, 35, 37, 39, 46, 47, 48, 49, 52, 54, 56, 59, 60, 63, 68, 70, 72, 74, 79, 83, 84, 87, 88, 89, 90, 91, 96), c(-101)] 
y.app_k <- data.train[, c(101)]
x.test_k <- data.test[, c(-101)]

# Chenxin:add scale -------------------------------------------------------

x.app_k.scale <- scale(data.train[c(6, 11, 12, 15, 17, 22, 23, 25, 27, 32, 33, 35, 37, 39, 46, 47, 48, 49, 52, 54, 56, 59, 60, 63, 68, 70, 72, 74, 79, 83, 84, 87, 88, 89, 90, 91, 96), c(-101)] )
x.test_k.scale <- scale(data.test[, c(-101)])


kmin <- 10
reg.knn1 <- knn.reg(train = x.app_k, test = x.test_k, y = y.app_k, k = kmin)
mse.knn1 <- mean((reg.knn1$pred - y.test) ^ 2) #4167, trop grande

# Chenxin K variation & scale -----------------------------------------------------------------

knn_k_max <- 37

tmp <- sapply(1:knn_k_max, function(local_k){
  reg.knn.noscale <- knn.reg(train = x.app_k, test = x.test_k, y = y.app_k, k = local_k)
  res <- mean((reg.knn.noscale$pred - y.test) ^ 2)
  return(res)
})

tmp.scale <- sapply(1:knn_k_max, function(local_k){
  reg.knn.scale <- knn.reg(train = x.app_k.scale, test = x.test_k.scale, y = y.app_k, k = local_k)
  res.scale <- mean((reg.knn.scale$pred - y.test) ^ 2)
  return(res.scale)
})

knn_MSE_noscale <- tmp
knn_MSE_scale <- tmp.scale
#erreur global
plot(1:knn_k_max, knn_MSE_noscale, 
     type='b', col='blue',
     ylim=range(knn_MSE_scale),
     xlab='k', ylab='MSE', lty = 1, pch = 1)

lines(1:knn_k_max, knn_MSE_scale, type='b', col='red', lty = 1, pch = 1)
which.min(knn_MSE_scale)
min(knn_MSE_scale)
which.min(knn_MSE_noscale)
min(knn_MSE_noscale)

#MSE Minimal in37 3707, MSE decrease with k increase  scale make no significatif difference


# End Code Chenxin ---------------



#Subset selection
#Vu qu'il existe trop de variables, nous pourrions pas utiliser la m??thode best subset selection

#forward selection
library(leaps)
library(dplyr)
reg.selection.forward <- regsubsets(y~., data = data.train, method = "forward", nbest = 1, nvmax = 100)
summary_forward <- summary(reg.selection.forward)
plot(reg.selection.forward, scale = "bic")#Regarder bri??vement la plus grande adjusted R Square

rss<-data.frame(summary_forward$outmat, RSS=summary_forward$rss)
rsquare_max_forward <- summary_forward$outmat[which.max(summary_forward$adjr2),]#La ligne avec la plus grande adjr2
rsquare_max_forward[rsquare_max_forward == '*'] <- as.numeric(1)
rsquare_max_forward[rsquare_max_forward == ' '] <- as.numeric(0)
rsquare_max_forward <- as.numeric(rsquare_max_forward)#Le masque pour s??lectionner les variables
reg.subset.forward <- reg.set[c(rsquare_max_forward==1)]


n.subset.forward <- nrow(reg.subset.forward)
set.seed(69)
n.subset.forward.train <- as.integer(train.percentage * n.subset.forward)
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
reg.subset.backward <- reg.set[c(rsquare_max_backward==1)]

n.subset.backward <- nrow(reg.subset.backward)
set.seed(69)
n.subset.backward.train <- as.integer(train.percentage * n.subset.backward)
n.subset.backward.sample <- sample(n.subset.backward, n.subset.backward.train)
reg.subset.backward.train <- reg.subset.backward[n.subset.backward.sample,]
reg.subset.backward.test <- reg.subset.backward[-n.subset.backward.sample,]
reg.subset.backward.lm <- lm(formula = y~., data = reg.subset.backward.train)
reg.subset.backward.lm.predict <- predict(reg.subset.backward.lm, newdata = reg.subset.backward.test)
reg.subset.backward.mse <- mean((reg.subset.backward.lm.predict - reg.subset.backward.test$y) ^ 2)#186.92
reg.subset.backward.err <- rstandard(reg.subset.backward.lm)
plot(reg.subset.backward.train$y, reg.subset.backward.err)
abline(0, 0)

#k-cross validation sur backward selection
#avec
Formula <- c(y~.-X2, 
             y~.-X2-X5, 
             y~.-X2-X5-X7, 
             y~.-X2-X5-X7-X8,
             y~.-X2-X5-X7-X8-X9,
             y~.-X2-X5-X7-X8-X9-X10, 
             y~.-X2-X5-X7-X8-X9-X10-X16, 
             y~.-X2-X5-X7-X8-X9-X10-X16-X18,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19, 
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20, 
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78-X79,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78-X79-X81,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78-X79-X81-X85,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78-X79-X81-X85-X93,
             y~.-X2-X5-X7-X8-X9-X10-X16-X18-X19-X20-X28-X30-X34-X38-X40-X41-X44-X53-X55-X57-X61-X64-X65-X66-X67-X73-X76-X77-X78-X79-X81-X85-X93-X97
)
K <- 10
fold <- sample(K, n_train, replace = TRUE)
CV <- rep(0, 10)
for (i in (1:10)){
  for (k in (1:K)){
    reg.cross<-lm(Formula[[i]],data=reg.set[fold!=k,])
    pred.cross <- predict(reg.cross, newdata=reg.set[fold == k,])
    CV[i]<-CV[i]+ sum((reg.set$y[fold==k]-pred.cross)^2)
  }
  CV[i]<-CV[i] / n_reg
}
CV.min = min(CV)#181

#Puisque la m??thode backward selection est mieux, 
#nous utilisons le regroupement des variables pour refaire k plus proches voisins
reg.knn2 <- knn.reg(train = reg.subset.backward.train[, c(-66)], test = reg.subset.backward.test[, c(-66)], y = reg.subset.backward.train[,c(66)], k = kmin)
mse.knn2 <- mean((reg.knn2$pred - y.test) ^ 2)#2226

#Préparation de données pour Ridge et Lasso
library(glmnet)
x<-model.matrix(y~.,reg.set)
y<-reg.set$y
data.train.regu <- x[id_train,]
y.train.regu <- y[id_train]
data.test.regu <- x[-id_train,]
y.test.regu <- y[-id_train]

#Ridge regression
cv.out.ridge <- cv.glmnet(data.train.regu, y.train.regu, alpha = 0)
plot(cv.out.ridge)
fit.ridge <- glmnet(data.train.regu, y.train.regu, lambda = cv.out.ridge$lambda.min, alpha = 0)
ridge.predict <- predict(fit.ridge, s = cv.out.ridge$lambda.min, newx = data.test.regu)
mse.ridge <- mean((ridge.predict - y.test.regu) ^ 2)#200.44

#lasso
cv.out.lasso <- cv.glmnet(data.train.regu, y.train.regu, alpha = 1)
plot(cv.out.lasso)
fit.lasso <- glmnet(data.train.regu, y.train.regu, lambda = cv.out.lasso$lambda.min, alpha = 1)
lasso.predict <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = data.test.regu)
mse.lasso <- mean((lasso.predict - y.test.regu) ^ 2)#178


regresseur <- function(dataset) {
  #load("env.Rdata")
  library(glmnet)
  train.percentage <- 2/3
  n_reg <- nrow(reg.set)
  n_train <- as.integer(n_reg * train.percentage)
  n_test <- n_reg - n_train
  set.seed(69)
  id_train <- sample(n_reg, n_train)
  
  
  x<-model.matrix(y~.,reg.set)
  y<-reg.set$y
  data.train.regu <- x[id_train,]
  y.train.regu <- y[id_train]
  data.test.regu <- x[-id_train,]
  y.test.regu <- y[-id_train]
  
  
  cv.out.lasso <- cv.glmnet(data.train.regu, y.train.regu, alpha = 1)
  plot(cv.out.lasso)
  fit.lasso <- glmnet(data.train.regu, y.train.regu, lambda = cv.out.lasso$lambda.min, alpha = 1)
  predictions <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = data.test.regu)
  mse.lasso <- mean((predictions - y.test.regu) ^ 2)#178
  print("La MSE est:")
  print(mse.lasso)
  return(predictions)
}
pred <- regresseur(reg.set)

