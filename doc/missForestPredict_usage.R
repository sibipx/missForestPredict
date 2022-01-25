## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(devtools)
#  devtools::install_git('https://gitlab.kuleuven.be/u0143313/missforestpredict/')
#  

## -----------------------------------------------------------------------------
library(missForestPredict)

## -----------------------------------------------------------------------------
data(iris)

N <- nrow(iris)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

iris_train <- iris[-id_test,]
iris_test <- iris[id_test,]


## -----------------------------------------------------------------------------
set.seed(2022)
iris_train_miss <- prodNA(iris_train, noNA = 0.1)
iris_test_miss <- prodNA(iris_test, noNA = 0.1)

head(iris_train_miss)
head(iris_test_miss)


## -----------------------------------------------------------------------------
set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, verbose = TRUE)


## -----------------------------------------------------------------------------
iris_train_imp <- iris_train_imp_object$ximp

head(iris_train_imp)


## -----------------------------------------------------------------------------
iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)


## -----------------------------------------------------------------------------
single_observation <- iris_test_miss[1,]
single_observation[1,2] <- NA

print(single_observation)

single_observation_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                               newdata = single_observation)

print(single_observation_imp)


## -----------------------------------------------------------------------------
single_observation <- iris_test_miss[2,]
single_observation[,1:4] <- NA_real_

print(single_observation)

single_observation_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                               newdata = single_observation)

print(single_observation_imp)


## -----------------------------------------------------------------------------
str(iris_train_imp_object, max.level = 1)


## -----------------------------------------------------------------------------
iris_train_imp <- iris_train_imp_object$ximp
iris_train_imp_object$ximp <- NULL 

iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)

## -----------------------------------------------------------------------------
library(ggplot2)

data(diamonds)

class(diamonds)

N <- nrow(diamonds)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

diamonds_train <- diamonds[-id_test,]
diamonds_test <- diamonds[id_test,]

diamonds_train_miss <- prodNA(diamonds_train, noNA = 0.1)
diamonds_test_miss <- prodNA(diamonds_test, noNA = 0.1)

head(diamonds_train_miss)
head(diamonds_test_miss)


## -----------------------------------------------------------------------------
set.seed(2022)
diamonds_train_imp_object <- missForestPredict::missForest(diamonds_train_miss, 
                                                           verbose = TRUE, 
                                                           num.trees = 100)

# impute test set
diamonds_train_imp_object$ximp <- NULL 
diamonds_test_imp <- missForestPredict::missForestPredict(diamonds_train_imp_object,
                                                          newdata = diamonds_test_miss)

head(diamonds_test_imp)


## -----------------------------------------------------------------------------
#set.seed(2022)
#diamonds_train_imp_object <- missForestPredict::missForest(diamonds_train_miss, verbose = TRUE, maxiter = 2)
#
## impute test set
#diamonds_train_imp_object$ximp <- NULL 
#diamonds_test_imp <- missForestPredict::missForestPredict(diamonds_train_imp_object, newdata = diamonds_test_miss)
#
#head(diamonds_test_imp)


