
# Classification ---------------------------------------
library(MASS)

clas.set <- read.csv("data/TPN1_a22_clas_app.txt", sep="")

n_clas <- dim(clas.set)[1]
train_percentage <- 4/5
n_train <- round(n_clas* train_percentage)
n_test <- n_clas - n_train

set.seed(19)
id_train <- sample(1:n_clas, n_train)
data.train <- clas.set[  id_train,]
data.test <- clas.set[- id_train,]
y.test <- clas.set[-id_train, c(51)]
y.train <- clas.set[id_train, c(51)]


data.train$y <- factor(data.train$y)

model.cls  <- naive_bayes(y ~ ., data=data.train)


prediction_cls <- function(dataset) {
  #load("env.Rdata")
  library(naivebayes)
  
  predictions <- predict(model.cls, newdata=dataset[1:50])
  perf.naiveqda <- table(dataset$y, predictions)
  perf.naiveqda
  err.naiveqda <- 1-sum(diag(perf.naiveqda)) / nrow(dataset)
  print("taux d'erreur avec Naive Bayes:")
  print(err.naiveqda)
  
  return(predictions)
}
pred <- prediction_cls(data.test)

# Regression -------------------------------------------

library(MASS)
reg.set <- read.table('data/TPN1_a22_reg_app.txt', header = TRUE)

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



x<-model.matrix(y~.,reg.set)
y<-reg.set$y
data.train.regu <- x[id_train,]
y.train.regu <- y[id_train]
data.test.regu <- x[-id_train,]
y.test.regu <- y[-id_train]


cv.out.lasso <- cv.glmnet(data.train.regu, y.train.regu, alpha = 1)
plot(cv.out.lasso)
model.reg <- glmnet(data.train.regu, y.train.regu, lambda = cv.out.lasso$lambda.min, alpha = 1)

prediction_reg <- function(dataset) {
  #load("env.Rdata")
  library(glmnet)
  
  dataset.x <- dataset[-101]
  
  dataset.matrix = cbind("(Intercept)" = rep(1,167),as.matrix(dataset.x))
  
  predictions <- predict(model.reg, s = 0.3176887, newx = dataset.matrix)
  mse.lasso <- mean((predictions - dataset$y) ^ 2)#178
  print("La MSE est:")
  print(mse.lasso)
  return(predictions)
}
pred <- prediction_reg(data.test)

save(
  "model.reg",
  "model.cls",
  "prediction_reg",
  "prediction_cls",
  file = "env.Rdata"
)

