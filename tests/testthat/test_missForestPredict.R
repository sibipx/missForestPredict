
context("missForestPredict")
library(missForestPredict)
library(testthat)

test_that("tibble returns tibble", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, save_models = TRUE, verbose = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object_tbl, newdata = iris_test_tbl)

  expect_is(iris_test_tbl_imp, "tbl_df")
  expect_is(missForest_object_tbl$ximp, "tbl_df")

})

test_that("dataframe returns dataframe", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  set.seed(2022)
  missForest_object_df <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE)
  iris_test_df_imp <- missForestPredict::missForestPredict(missForest_object_df, newdata = iris_test)

  expect_is(iris_test_df_imp, "data.frame")
  expect_is(missForest_object_df$ximp, "data.frame")

})

test_that("tibble and dataframe results are the same for missForest", {

  require(tidyverse)

  data(iris)
  iris_mis <- produce_NA(iris, proportion = 0.1)
  iris_mis_tbl <- as_tibble(iris_mis)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_mis, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_mis_tbl, save_models = TRUE, verbose = FALSE)
  iris_imp_tbl <- missForest_object_tbl$ximp

  expect_equal(iris_imp_tbl, as_tibble(iris_imp_df))

})

test_that("tibble and dataframe results are the same for missForestPredict", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE)
  iris_test_df_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, save_models = TRUE, verbose = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test_tbl)

  expect_equal(iris_test_tbl_imp, as_tibble(iris_test_df_imp))

})

test_that("prediction on training set is the same as imputation on dataframe", {

  data(iris)
  set.seed(2022)
  iris_mis <- produce_NA(iris, proportion = 0.1)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_mis, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  missForest_predictions <- missForestPredict::missForestPredict(missForest_object, newdata = iris_mis)

  expect_equal(iris_imp_df, missForest_predictions)

})

test_that("prediction on training set is the same as imputation on tibble", {

  data(iris)
  set.seed(2022)
  iris_mis <- produce_NA(iris, proportion = 0.1)
  iris_mis <- as_tibble(iris_mis)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_mis, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  missForest_predictions <- missForestPredict::missForestPredict(missForest_object, newdata = iris_mis)

  expect_equal(iris_imp_df, missForest_predictions)

})

test_that("imputing complete dataframe returns the same dataframe", {

  data(iris)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  expect_equal(iris_imp_df, iris)

})

test_that("imputing complete tibble returns the same tibble", {

  data(iris)

  set.seed(2022)
  iris <- as_tibble(iris)
  missForest_object <- missForestPredict::missForest(iris, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  expect_equal(iris_imp_df, iris)

})

test_that("imputation is the same for factor and character", {

  data(iris)

  iris <- produce_NA(iris, proportion = 0.2)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris, save_models = TRUE, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  iris_char <- iris
  iris_char$Species <- as.character(iris_char$Species)

  set.seed(2022)
  missForest_object_char <- missForestPredict::missForest(iris_char, save_models = TRUE, verbose = FALSE)
  iris_imp_df_char <- missForest_object_char$ximp

  iris_imp_df_char$Species <- as.factor(iris_imp_df_char$Species)

  expect_equal(iris_imp_df, iris_imp_df_char)

})

test_that("integer is returned as double when return_integer_as_integer is FALSE", {

  require(tidyverse)

  data(iris)
  set.seed(2022)

  iris$Sepal.Length <- as.integer(iris$Sepal.Length)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  # impute train and test df
  set.seed(2022)
  missForest_object_df <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE,
                                                        return_integer_as_integer = FALSE)
  iris_test_df_imp <- missForestPredict::missForestPredict(missForest_object_df, newdata = iris_test)

  # impute train and test tibble
  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, save_models = TRUE, verbose = FALSE,
                                                         return_integer_as_integer = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object_tbl, newdata = iris_test_tbl)

  expect_type(iris_test_df_imp$Sepal.Length, "double")
  expect_type(missForest_object_df$ximp$Sepal.Length, "double")

  expect_type(iris_test_tbl_imp$Sepal.Length, "double")
  expect_type(missForest_object_tbl$ximp$Sepal.Length, "double")

})

test_that("integer is returned as integer when return_integer_as_integer is TRUE", {

  require(tidyverse)

  data(iris)
  set.seed(2022)

  iris$Sepal.Length <- as.integer(iris$Sepal.Length)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  # impute train and test df
  set.seed(2022)
  missForest_object_df <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE,
                                                        return_integer_as_integer = TRUE)
  iris_test_df_imp <- missForestPredict::missForestPredict(missForest_object_df, newdata = iris_test)

  # impute train and test tibble
  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, save_models = TRUE, verbose = FALSE,
                                                         return_integer_as_integer = TRUE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object_tbl, newdata = iris_test_tbl)

  expect_type(iris_test_df_imp$Sepal.Length, "integer")
  expect_type(missForest_object_df$ximp$Sepal.Length, "integer")

  expect_type(iris_test_tbl_imp$Sepal.Length, "integer")
  expect_type(missForest_object_tbl$ximp$Sepal.Length, "integer")

})

test_that("re-imputing with missForestPredict gives the same results as missForest", {

  require(tidyverse)

  data(iris)
  iris_miss <- produce_NA(iris, proportion = 0.1)
  iris_miss_tbl <- as_tibble(iris_miss)

  missForest_object <- missForestPredict::missForest(iris_miss, save_models = TRUE, verbose = FALSE)
  missForest_object_tbl <- missForestPredict::missForest(iris_miss_tbl, save_models = TRUE, verbose = FALSE)
  # re-impute
  iris_miss_imp <- missForestPredict::missForestPredict(missForest_object,
                                                        newdata = iris_miss)
  iris_miss_imp_tbl <- missForestPredict::missForestPredict(missForest_object_tbl,
                                                            newdata = iris_miss_tbl)

  expect_equal(missForest_object$ximp, iris_miss_imp)
  expect_equal(missForest_object_tbl$ximp, iris_miss_imp_tbl)

})

test_that("re-imputing with missForestPredict gives the same results as missForest - 1 iteration", {

  require(tidyverse)

  data(iris)
  iris_miss <- produce_NA(iris, proportion = 0.1)
  iris_miss_tbl <- as_tibble(iris_miss)

  missForest_object <- missForestPredict::missForest(iris_miss, save_models = TRUE, verbose = FALSE,
                                                     maxiter = 1)
  missForest_object_tbl <- missForestPredict::missForest(iris_miss_tbl, save_models = TRUE, verbose = FALSE,
                                                         maxiter = 1)
  # re-impute
  iris_miss_imp <- missForestPredict::missForestPredict(missForest_object,
                                                        newdata = iris_miss)
  iris_miss_imp_tbl <- missForestPredict::missForestPredict(missForest_object_tbl,
                                                            newdata = iris_miss_tbl)

  expect_equal(missForest_object$ximp, iris_miss_imp)
  expect_equal(missForest_object_tbl$ximp, iris_miss_imp_tbl)

})

test_that("infinite values result in error", {

  data(iris)
  iris_miss <- produce_NA(iris, proportion = 0.1)
  iris_miss[1,1] <- Inf

  expect_error(missForestPredict::missForest(iris_miss, verbose = FALSE))

})

test_that("for variable with 1 class a single value model is learned", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_equal(all(iris_test_imp$Species[is.na(iris_test$Species)] == "versicolor"),
               TRUE)

})

test_that("for variable with 1 class a single value model is learned", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_equal(all(iris_test_imp$Species[is.na(iris_test$Species)] == "versicolor"),
               TRUE)

})

test_that("excluding a variable from imputation works (predictor matrix)", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  predictor_matrix <- create_predictor_matrix(iris_train)
  predictor_matrix["Sepal.Length",] <- 0

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE,
                                                     predictor_matrix = predictor_matrix,
                                                     verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_null(missForest_object$models[[1]]$Sepal.Length)
  expect_equal(missForest_object$models[[1]]$Sepal.Width$num.independent.variables, 4)
  expect_equal(missForest_object$models[[1]]$Petal.Length$num.independent.variables, 4)
  expect_equal(missForest_object$models[[1]]$Petal.Width$num.independent.variables, 4)
  expect_equal(missForest_object$models[[1]]$Species$num.independent.variables, 4)
  expect_equal(is.null(missForest_object$init$Sepal.Length), FALSE)

})

test_that("excluding a variable as a predictor works (predictor matrix)", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  predictor_matrix <- create_predictor_matrix(iris_train)
  predictor_matrix[,"Sepal.Length"] <- 0

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE,
                                                     predictor_matrix = predictor_matrix,
                                                     verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_equal(is.null(missForest_object$models[[1]]$Sepal.Length), FALSE)
  expect_equal(missForest_object$models[[1]]$Sepal.Width$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Petal.Length$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Petal.Width$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Species$num.independent.variables, 3)
  expect_equal(is.null(missForest_object$init$Sepal.Length), FALSE)
  expect_equal(sort(missForest_object$models[[1]]$Sepal.Length$forest$independent.variable.names),
               sort(names(predictor_matrix["Sepal.Length",][predictor_matrix["Sepal.Length",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Sepal.Width$forest$independent.variable.names),
               sort(names(predictor_matrix["Sepal.Width",][predictor_matrix["Sepal.Width",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Petal.Length$forest$independent.variable.names),
               sort(names(predictor_matrix["Petal.Length",][predictor_matrix["Petal.Length",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Petal.Width$forest$independent.variable.names),
               sort(names(predictor_matrix["Petal.Width",][predictor_matrix["Petal.Width",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Species$forest$independent.variable.names),
               sort(names(predictor_matrix["Species",][predictor_matrix["Species",] == 1])))
})

test_that("excluding a variable from imputation and as a predictor works (predictor matrix)", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)

  predictor_matrix <- create_predictor_matrix(iris_train)
  predictor_matrix[,"Sepal.Length"] <- 0
  predictor_matrix["Sepal.Length",] <- 0

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE,
                                                     predictor_matrix = predictor_matrix,
                                                     verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_null(missForest_object$models[[1]]$Sepal.Length)
  expect_equal(missForest_object$models[[1]]$Sepal.Width$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Petal.Length$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Petal.Width$num.independent.variables, 3)
  expect_equal(missForest_object$models[[1]]$Species$num.independent.variables, 3)
  expect_null(missForest_object$init$Sepal.Length)
  expect_equal(sort(missForest_object$models[[1]]$Sepal.Width$forest$independent.variable.names),
               sort(names(predictor_matrix["Sepal.Width",][predictor_matrix["Sepal.Width",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Petal.Length$forest$independent.variable.names),
               sort(names(predictor_matrix["Petal.Length",][predictor_matrix["Petal.Length",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Petal.Width$forest$independent.variable.names),
               sort(names(predictor_matrix["Petal.Width",][predictor_matrix["Petal.Width",] == 1])))
  expect_equal(sort(missForest_object$models[[1]]$Species$forest$independent.variable.names),
               sort(names(predictor_matrix["Species",][predictor_matrix["Species",] == 1])))
})

test_that("imputation works when train and test have different factor levels", {

  data(iris)

  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_train$Species <- "versicolor"
  iris_train$Species <- as.factor(iris_train$Species)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
  iris_test$Species <- as.factor(as.character(iris_test$Species))

  # impute train and test df
  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, save_models = TRUE, verbose = FALSE)
  iris_test_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  expect_equal(all(iris_test_imp$Species[is.na(iris_test$Species)] == "versicolor"),
               TRUE)

})
