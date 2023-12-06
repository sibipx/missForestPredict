## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  library(devtools)
#  devtools::install_git('https://gitlab.kuleuven.be/u0143313/missforestpredict/', dependencies = TRUE)
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
iris_train_miss <- produce_NA(iris_train, proportion = 0.1)
iris_test_miss <- produce_NA(iris_test, proportion = 0.1)

head(iris_train_miss)
head(iris_test_miss)


## -----------------------------------------------------------------------------
set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, save_models = TRUE)


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
str(iris_train_imp_object, max.level = 1)


## -----------------------------------------------------------------------------
iris_train_imp <- iris_train_imp_object$ximp
iris_train_imp_object$ximp <- NULL 

iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)

## -----------------------------------------------------------------------------
set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, save_models = TRUE)

# store imputed dataframe
iris_train_imp <- iris_train_imp_object$ximp
iris_train_imp_object$ximp <- NULL 

# re-impute the same dataframe using missForestPredict
iris_train_imp_2 <-  missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_train_miss)

identical(iris_train_imp, iris_train_imp_2)

## -----------------------------------------------------------------------------
library(ggplot2)

data(diamonds)

# split train / test
N <- nrow(diamonds)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

diamonds_train <- diamonds[-id_test,]
diamonds_test <- diamonds[id_test,]

diamonds_train_miss <- produce_NA(diamonds_train, proportion = 0.1)
diamonds_test_miss <- produce_NA(diamonds_test, proportion = 0.1)

head(diamonds_train_miss)
head(diamonds_test_miss)


## -----------------------------------------------------------------------------
set.seed(2022)
diamonds_train_imp_object <- missForestPredict::missForest(diamonds_train_miss,
                                                           save_models = TRUE,
                                                           num.trees = 100)

# impute test set
diamonds_train_imp_object$ximp <- NULL 
diamonds_test_imp <- missForestPredict::missForestPredict(diamonds_train_imp_object,
                                                          newdata = diamonds_test_miss)

head(diamonds_test_imp)


## -----------------------------------------------------------------------------
data(iris)

# split train / test
N <- nrow(iris)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

iris_train <- iris[-id_test,]
iris_test <- iris[id_test,]

# produce missing values
set.seed(2022)
iris_train_miss <- produce_NA(iris_train, proportion = c(0.2, 0, 0.2, 0.2, 0))
iris_test_miss <- produce_NA(iris_test, proportion = c(0.2, 0, 0.2, 0.2, 0))

# build linear models for Sepal.Length, Petal.Width, Petal.Length using complete cases for each variable
fit_1 <- lm(Sepal.Length ~ ., data = iris_train_miss[!is.na(iris_train_miss$Sepal.Length), 
                                                     c("Sepal.Length", "Sepal.Width", "Species")])
fit_2 <- lm(Petal.Width ~ ., data = iris_train_miss[!is.na(iris_train_miss$Petal.Width), 
                                                    c("Petal.Width", "Sepal.Width", "Species")])
fit_3 <- lm(Petal.Length ~ ., data = iris_train_miss[!is.na(iris_train_miss$Petal.Length), 
                                                     c("Petal.Length", "Sepal.Width", "Species")])

# impute training with predictions of linear model
iris_train_init <- iris_train_miss
iris_train_init$Sepal.Length[is.na(iris_train_init$Sepal.Length)] <- 
  predict(fit_1, iris_train_init[is.na(iris_train_init$Sepal.Length), c("Sepal.Width", "Species")])
iris_train_init$Petal.Width[is.na(iris_train_init$Petal.Width)] <- 
  predict(fit_2, iris_train_init[is.na(iris_train_init$Petal.Width), c("Sepal.Width", "Species")])
iris_train_init$Petal.Length[is.na(iris_train_init$Petal.Length)] <- 
  predict(fit_3, iris_train_init[is.na(iris_train_init$Petal.Length), c("Sepal.Width", "Species")])

# impute the training set using this initialization
set.seed(2022)
iris_train_imp_obj <- missForest(iris_train_miss, 
                                 save_models = TRUE,
                                 initialization = "custom", 
                                 x_init = iris_train_init)

# build test set initialization using the linear models learned on training
iris_test_init <- iris_test_miss
iris_test_init$Sepal.Length[is.na(iris_test_init$Sepal.Length)] <- 
  predict(fit_1, iris_test_init[is.na(iris_test_init$Sepal.Length), c("Sepal.Width", "Species")])
iris_test_init$Petal.Width[is.na(iris_test_init$Petal.Width)] <- 
  predict(fit_2, iris_test_init[is.na(iris_test_init$Petal.Width), c("Sepal.Width", "Species")])
iris_test_init$Petal.Length[is.na(iris_test_init$Petal.Length)] <- 
  predict(fit_3, iris_test_init[is.na(iris_test_init$Petal.Length), c("Sepal.Width", "Species")])

# impute test set
iris_test_imp <- missForestPredict(iris_train_imp_obj, newdata = iris_test_miss, 
                                   x_init = iris_test_init)

evaluate_imputation_error(iris_test_imp, iris_test_miss, iris_test)
evaluate_imputation_error(iris_test_init, iris_test_miss, iris_test)


## -----------------------------------------------------------------------------
data(iris)

iris$Date_collected <- seq(Sys.Date() - nrow(iris) + 1, Sys.Date(), by="days")

# split train / test
N <- nrow(iris)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

iris_train <- iris[-id_test,]
iris_test <- iris[id_test,]

iris_train_miss <- produce_NA(iris_train, proportion = c(0.1,0.1,0.1,0.1,0.1,0))
iris_test_miss <- produce_NA(iris_test, proportion = c(0.1,0.1,0.1,0.1,0.1,0))

head(iris_train_miss)


## -----------------------------------------------------------------------------

predictor_matrix <- create_predictor_matrix(iris_train_miss)

print(predictor_matrix)


## -----------------------------------------------------------------------------

predictor_matrix["Date_collected",] <- 0
predictor_matrix[,"Date_collected"] <- 0

print(predictor_matrix)


## -----------------------------------------------------------------------------

set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, save_models = TRUE, 
                                                       predictor_matrix = predictor_matrix,
                                                       verbose = TRUE)

iris_train_imp <- iris_train_imp_object$ximp

head(iris_train_imp)

iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)

## -----------------------------------------------------------------------------
data(iris)

# split train / test
N <- nrow(iris)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

iris_train <- iris[-id_test,]
iris_test <- iris[id_test,]

iris_train_miss <- produce_NA(iris_train, proportion = 0.1)
iris_test_miss <- produce_NA(iris_test, proportion = 0.1)

head(iris_train_miss)


## -----------------------------------------------------------------------------

predictor_matrix <- create_predictor_matrix(iris_train_miss)
predictor_matrix["Sepal.Length",] <- 0

print(predictor_matrix)


## -----------------------------------------------------------------------------

set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, save_models = TRUE, 
                                                       predictor_matrix = predictor_matrix,
                                                       verbose = TRUE)

iris_train_imp <- iris_train_imp_object$ximp

iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)

## -----------------------------------------------------------------------------
names(iris_train_imp_object$models[[1]])
names(iris_train_imp_object$init)

## -----------------------------------------------------------------------------
data(iris)

# split train / test
N <- nrow(iris)
n_test <- floor(N/3)

set.seed(2022)
id_test <- sample(1:N, n_test)

iris_train <- iris[-id_test,]
iris_test <- iris[id_test,]

iris_train_miss <- produce_NA(iris_train, proportion = 0.1)
iris_test_miss <- produce_NA(iris_test, proportion = 0.1)

head(iris_train_miss)

predictor_matrix <- create_predictor_matrix(iris_train_miss)
predictor_matrix["Petal.Length","Species"] <- 0
predictor_matrix["Petal.Width","Species"] <- 0

print(predictor_matrix)

set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_train_miss, save_models = TRUE, 
                                                       predictor_matrix = predictor_matrix,
                                                       verbose = TRUE)

iris_train_imp <- iris_train_imp_object$ximp

iris_test_imp <- missForestPredict::missForestPredict(iris_train_imp_object, 
                                                      newdata = iris_test_miss)

head(iris_test_imp)

## -----------------------------------------------------------------------------
check_predictor_matrix(predictor_matrix, iris_train)


## -----------------------------------------------------------------------------

data(iris)

iris_mis <- iris
iris_mis[1:50, "Sepal.Length"] <- NA
iris_mis[1:50, "Sepal.Width"] <- NA
iris_mis[51:150, "Petal.Length"] <- NA

set.seed(2022)
iris_train_imp_object <- missForestPredict::missForest(iris_mis, save_models = TRUE, 
                                                       verbose = TRUE)

print(iris_train_imp_object$predictor_matrix)

check_predictor_matrix(iris_train_imp_object$predictor_matrix, iris_mis, verbose = TRUE)


