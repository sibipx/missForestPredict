
context("missForestPredict")
library(missForestPredict)
library(testthat)

test_that("tibble retruns tibble", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- prodNA(iris[1:100,], noNA = 0.1)
  iris_test <- prodNA(iris[101:150,], noNA = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, verbose = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object_tbl, newdata = iris_test_tbl)

  expect_is(iris_test_tbl_imp, "tbl_df")
  expect_is(missForest_object_tbl$ximp, "tbl_df")

})

test_that("tibble and dataframe results are the same for missForest", {

  require(tidyverse)

  data(iris)
  iris_mis <- prodNA(iris, noNA = 0.1)
  iris_mis_tbl <- as_tibble(iris_mis)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_mis, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_mis_tbl, verbose = FALSE)
  iris_imp_tbl <- missForest_object_tbl$ximp

  expect_equal(iris_imp_tbl, as_tibble(iris_imp_df))

})

test_that("tibble and dataframe results are the same for missForestPredict", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- prodNA(iris[1:100,], noNA = 0.1)
  iris_test <- prodNA(iris[101:150,], noNA = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_train, verbose = FALSE)
  iris_test_df_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test)

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, verbose = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object, newdata = iris_test_tbl)

  expect_equal(iris_test_tbl_imp, as_tibble(iris_test_df_imp))

})

test_that("tibble retruns tibble", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- prodNA(iris[1:100,], noNA = 0.1)
  iris_test <- prodNA(iris[101:150,], noNA = 0.1)
  iris_train_tbl <- as_tibble(iris_train)
  iris_test_tbl <- as_tibble(iris_test, rownames = NA)

  set.seed(2022)
  missForest_object_tbl <- missForestPredict::missForest(iris_train_tbl, verbose = FALSE)
  iris_test_tbl_imp <- missForestPredict::missForestPredict(missForest_object_tbl, newdata = iris_test_tbl)

  expect_is(iris_test_tbl_imp, "tbl_df")
  expect_is(missForest_object_tbl$ximp, "tbl_df")

})

test_that("prediction on training set is the same as imputation", {

  data(iris)
  set.seed(2022)
  iris_mis <- prodNA(iris, noNA = 0.1)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris_mis, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  missForest_predictions <- missForestPredict::missForestPredict(missForest_object, newdata = iris_mis)

  expect_equal(iris_imp_df, missForest_predictions)

})

test_that("imputing complete dataframe returns the same dataframe", {

  data(iris)

  set.seed(2022)
  missForest_object <- missForestPredict::missForest(iris, verbose = FALSE)
  iris_imp_df <- missForest_object$ximp

  expect_equal(iris_imp_df, iris)

})
