
# Classification ---------------------------------------
library(naivebayes)
library(MASS)


clas.set <- read.csv("data/TPN1_a22_clas_app.txt", sep="")

n_clas <- dim(clas.set)[1]
train_percentage <- 4/5
n_train <- round(n_clas* train_percentage)
n_test <- n_clas - n_train

set.seed(69)
id_train <- sample(n_clas, n_train)
data.test.cls <- clas.set[- id_train,] # for us to have a test set


clas.set$y <- factor(clas.set$y)

model.cls  <- qda(y ~ ., data=clas.set)


prediction_cls <- function(dataset) {
  library(MASS)
  predictions <- predict(model.cls, newdata=dataset[1:50])
  return(predictions$class)
}
pred.cls <- prediction_cls(data.test.cls)

# Regression -------------------------------------------

#Préparation de données pour Lasso

reg.set <- read.table('data/TPN1_a22_reg_app.txt')

train.percentage <- 2/3
n_reg <- nrow(reg.set)
n_train <- as.integer(n_reg * train.percentage)
n_test <- n_reg - n_train
set.seed(69)
id_train <- sample(n_reg, n_train)

data.test.reg <- reg.set[-id_train,] # for us to have a test set


library(glmnet)
library(Matrix)
x<-model.matrix(y~.,reg.set)
y<-reg.set$y

cv.out.lasso <- cv.glmnet(x, y, alpha = 1)
plot(cv.out.lasso)
model.reg <- glmnet(x, y, lambda = cv.out.lasso$lambda.min, alpha = 1)
cv.out.lasso <- cv.glmnet(x, y, alpha = 1)


prediction_reg <- function(dataset) {
  library(glmnet)
  library(Matrix)
  names(dataset)[length(dataset)]<-"y" 
  x<-model.matrix(y~.,dataset)
  x <- cbind(x,rep(1,nrow(dataset)))
  predictions <- predict(model.reg, s = 0.2419425, newx = x) # the value is lambda min
  print(mean((predictions - dataset$y) ^ 2))
  return(as.numeric(predictions))
}

pred.reg <- prediction_reg(data.test.reg)

# without test set (to submit)
save(
  "model.reg",
  "model.cls",
  "prediction_reg",
  "prediction_cls",
  file = "env.Rdata"
)

# with the test set (for us)
save(
  "model.reg",
  "model.cls",
  "data.test.reg",
  "data.test.cls",
  "prediction_reg",
  "prediction_cls",
  file = "env_test.Rdata"
)

