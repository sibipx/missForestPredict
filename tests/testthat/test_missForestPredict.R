
context("missForestPredict")
library(missForestPredict)
library(testthat)

test_that("tibble retruns tibble", {

  require(tidyverse)

  data(iris)
  set.seed(2022)
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
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
  iris_mis <- produce_NA(iris, proportion = 0.1)
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
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
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
  iris_train <- produce_NA(iris[1:100,], proportion = 0.1)
  iris_test <- produce_NA(iris[101:150,], proportion = 0.1)
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
  iris_mis <- produce_NA(iris, proportion = 0.1)

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

test_that("imputation is the same for factor and character", {

  data(iris)

  iris <- produce_NA(iris, proportion = 0.2)

  missForest_object <- missForestPredict::missForest(iris, verbose = FALSE, seed = 2022)
  iris_imp_df <- missForest_object$ximp

  iris_char <- iris
  iris_char$Species <- as.character(iris_char$Species)

  missForest_object_char <- missForestPredict::missForest(iris_char, verbose = FALSE, seed = 2022)
  iris_imp_df_char <- missForest_object_char$ximp

  iris_imp_df_char$Species <- as.factor(iris_imp_df_char$Species)

  expect_equal(iris_imp_df, iris_imp_df_char)

})
