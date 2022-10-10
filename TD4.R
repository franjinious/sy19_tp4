library(MASS)
reg <- read.table('D:/ÎÄµµ/SY19/TD/TD4/TPN1_a22_reg_app.txt', header = TRUE)
class <- read.table('D:/ÎÄµµ/SY19/TD/TD4/TPN1_a22_clas_app.txt', header = TRUE)

plot(reg[,1:10], reg$y)
percentage <- 2/3

n_reg <- nrow(reg)
ntrain_reg <- as.integer(n_reg * percentage)
ntest_reg <- n_reg - ntrain_reg
set.seed(69)
train_reg <- sample(n_reg, ntrain_reg)
x.app_reg <- reg[train_reg,]
x.test_reg <- reg[-train_reg,]
y.test_reg <- reg[-train_reg, c(101)]
y.app_reg <- reg[train_reg, c(101)]

boxplot(as.data.frame(reg[1:100]))

#pca
library(pls)
pcr_model <- pcr(y~., data = x.test_reg, validation = "CV")
validationplot(pcr_model, val.type="MSEP")


#modele lineare#
reg.lm <- lm(formula = y ~., data = x.app_reg)
summary(reg.lm)
res_std <- rstandard(reg.lm)
plot(x = y.app_reg, y = res_std)
abline(0, 0)
pre.lm <- predict(reg.lm, newdata = x.test_reg)
mse.lm <- mean((pre.lm - y.test_reg) ^ 2)
#var.mse <- var(mse.lm)


#k plus proches voisins
library(FNN)
x.app_k <- x.app_reg[c(6, 11, 12, 15, 17, 22, 23, 25, 27, 32, 33, 35, 37, 39, 46, 47, 48, 49, 52, 54, 56, 59, 60, 63, 68, 70, 72, 74, 79, 83, 84, 87, 88, 89, 90, 91, 96), c(-101)] 
y.app_k <- x.app_reg[, c(101)]
x.test_k <- x.test_reg[, c(-101)]
kmin <- 10
reg.subset.forward.knn.x <- reg[c(rsquare_max_forward==1), c(-101)]
reg.knn1 <- knn.reg(train = x.app_k, test = x.test_k, y = y.app_k, k = kmin)
reg.knn2 <- knn.reg(train = reg.subset.backward.train[, c(-66)], test = reg.subset.backward.test[, c(-66)], y = reg.subset.backward.train[,c(66)], k = kmin)
mse.knn1 <- mean((reg.knn1$pred - y.test_reg) ^ 2)


#Subset selection
#Vu qu'il existe trop de variables, nous pourrions pas utiliser la m¨¦thode best subset selection

#forward selection
library(leaps)
library(dplyr)
reg.selection.forward <- regsubsets(y~., data = x.app_reg, method = "forward", nbest = 1, nvmax = 100)
summary_forward <- summary(reg.selection.forward)
plot(reg.selection.forward, scale = "r2")
rss<-data.frame(summary_forward$outmat, RSS=summary_forward$rss)
rsquare_max_forward <- summary_forward$outmat[match(c(max(summary_forward$adjr2)),summary_forward$adjr2),]
rsquare_max_forward[rsquare_max_forward == '*'] <- as.numeric(1)
rsquare_max_forward[rsquare_max_forward == ' '] <- as.numeric(0)
rsquare_max_forward <- as.numeric(rsquare_max_forward)
reg.subset.forward <- reg[c(rsquare_max_forward==1)]

n.subset.forward <- nrow(reg.subset.forward)
set.seed(69)
n.subset.forward.train <- as.integer(percentage * n.subset.forward)
n.subset.forward.sample <- sample(n.subset.forward, n.subset.forward.train)
reg.subset.forward.train <- reg.subset.forward[n.subset.forward.sample,]
reg.subset.forward.test <- reg.subset.forward[-n.subset.forward.sample,]
reg.subset.forward.lm <- lm(formula = y~., data = reg.subset.forward.train)
reg.subset.forward.lm.predict <- predict(reg.subset.forward.lm, newdata = reg.subset.forward.test)
reg.subset.forward.mse <- mean((reg.subset.forward.lm.predict - reg.subset.forward.test$y) ^ 2)
reg.subset.forward.err <- rstandard(reg.subset.forward.lm)
plot(reg.subset.forward.train$y, reg.subset.forward.err)
abline(0, 0)

#backward selection
reg.selection.backward <- regsubsets(y~., data = x.app_reg, method = "backward", nbest = 1, nvmax = 100)
summary_backward <- summary(reg.selection.backward)
plot(reg.selection.backward, scale = "r2")
#rss<-data.frame(summary_forward$outmat, RSS=summary_forward$rss)
rsquare_max_backward <- summary_backward$outmat[match(c(max(summary_backward$adjr2)),summary_backward$adjr2),]
rsquare_max_backward[rsquare_max_backward == '*'] <- as.numeric(1)
rsquare_max_backward[rsquare_max_backward == ' '] <- as.numeric(0)
rsquare_max_backward <- as.numeric(rsquare_max_backward)
reg.subset.backward <- reg[c(rsquare_max_backward==1)]

n.subset.backward <- nrow(reg.subset.backward)
set.seed(69)
n.subset.backward.train <- as.integer(percentage * n.subset.backward)
n.subset.backward.sample <- sample(n.subset.backward, n.subset.backward.train)
reg.subset.backward.train <- reg.subset.backward[n.subset.backward.sample,]
reg.subset.backward.test <- reg.subset.backward[-n.subset.backward.sample,]
reg.subset.backward.lm <- lm(formula = y~., data = reg.subset.backward.train)
reg.subset.backward.lm.predict <- predict(reg.subset.backward.lm, newdata = reg.subset.backward.test)
reg.subset.backward.mse <- mean((reg.subset.backward.lm.predict - reg.subset.backward.test$y) ^ 2)
reg.subset.backward.err <- rstandard(reg.subset.backward.lm)
plot(reg.subset.backward.train$y, reg.subset.backward.err)
abline(0, 0)

#Puisque la m¨¦thode backward selection est mieux, 
#nous utilisons le regroupement des variables pour refaire k plus proches voisins
reg.knn2 <- knn.reg(train = reg.subset.backward.train[, c(-66)], test = reg.subset.backward.test[, c(-66)], y = reg.subset.backward.train[,c(66)], k = kmin)
mse.knn2 <- mean((reg.knn2$pred - y.test_reg) ^ 2)

#Ridge regression
library(glmnet)
x<-model.matrix(y~.,reg)
y<-reg$y
x.train <- x[train_reg,]
y.train <- y[train_reg]
x.test <- x[-train_reg,]
y.test <- y[-train_reg]

cv.out.ridge <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out.ridge)
fit.ridge <- glmnet(x.train, y.train, lambda = cv.out.ridge$lambda.min, alpha = 0)
ridge.predict <- predict(fit.ridge, s = cv.out$lambda.min, newx = x.test)
mse.ridge <- mean((ridge.predict - y.test) ^ 2)

#lasso
cv.out.lasso <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out.lasso)
fit.lasso <- glmnet(x.train, y.train, lambda = cv.out.lasso$lambda.min, alpha = 1)
lasso.predict <- predict(fit.lasso, s = cv.out.lasso$lambda.min, newx = x.test)
mse.lasso <- mean((lasso.predict - y.test) ^ 2)


classifieur <- function(dataset) {
  
  # Chargement de l¡¯environnement
  load("env.Rdata")
  # Mon algorithme qui renvoie les pr¨¦dictions sur le jeu de donn¨¦es
  # ¡®dataset¡® fourni en argument.
  # ...
  return(predictions)
}


regresseur <- function(dataset) {
  # Chargement de l¡¯environnement
  load("env.Rdata")
  # Mon algorithme qui renvoie les pr¨¦dictions sur le jeu de donn¨¦es
  # ¡®dataset¡® fourni en argument.
  # ...
  return(predictions)
}
