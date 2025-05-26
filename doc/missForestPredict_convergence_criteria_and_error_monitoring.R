## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(missForestPredict)

data(iris)
set.seed(2022)
iris_mis <- produce_NA(iris, proportion = 0.5)

set.seed(2022)
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE, num.threads = 2)

print(missForest_object$OOB_err)


## ----fig.width = 7, message=FALSE---------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

missForest_object$OOB_err %>% 
  filter(!is.na(NMSE)) %>% 
  ggplot(aes(iteration, NMSE, col = variable)) +
  geom_point() +
  geom_line()


## ----fig.width = 7------------------------------------------------------------
library(missForestPredict)
library(dplyr)
library(tidyr)
library(ggplot2)

data(iris)

proportion_missing <- c(0, 0, 0, 0.3, 0.3)

set.seed(2022)
iris_mis <- produce_NA(iris, proportion = proportion_missing)

set.seed(2022)
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE, num.threads = 2)

# plot convergence
missForest_object$OOB_err %>% 
  filter(!is.na(NMSE)) %>% 
  ggplot(aes(iteration, NMSE, col = variable)) +
  geom_point() +
  geom_line()


## ----fig.width = 7------------------------------------------------------------

set.seed(2022)
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE, 
                                                   var_weights = setNames(rep(1, ncol(iris_mis)), colnames(iris_mis)), num.threads = 2)

# plot convergence
missForest_object$OOB_err %>% 
  filter(!is.na(NMSE)) %>% 
  ggplot(aes(iteration, NMSE, col = variable)) +
  geom_point() +
  geom_line()


## -----------------------------------------------------------------------------
evaluate_imputation_error(missForest_object$ximp, iris_mis, iris)


