## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(devtools)
devtools::install_git('https://gitlab.kuleuven.be/u0143313/missforestpredict/', dependencies = TRUE)

library(missForestPredict)

data(iris)
set.seed(2022)
iris_mis <- produce_NA(iris, proportion = 0.5)

set.seed(2022)
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE)

print(missForest_object$err_MSE)

print(missForest_object$err_NMSE)



## ----fig.width = 7------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

col_names <- colnames(missForest_object$err_NMSE)

missForest_object$err_NMSE %>% 
  mutate(iteration = row_number()) %>% 
  pivot_longer(cols = all_of(col_names),
               values_to = "NMSE") %>% 
  ggplot(aes(iteration, NMSE, col = name)) +
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
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE)

# plot convergence
col_names <- colnames(missForest_object$err_NMSE)

missForest_object$err_NMSE %>% 
  mutate(iteration = row_number()) %>% 
  pivot_longer(cols = all_of(col_names),
               values_to = "NMSE") %>% 
  ggplot(aes(iteration, NMSE, col = name)) +
  geom_point() +
  geom_line()



## ----fig.width = 7------------------------------------------------------------

set.seed(2022)
missForest_object <- missForestPredict::missForest(iris_mis, verbose = TRUE, OOB_weights = proportion_missing)

# plot convergence
col_names <- colnames(missForest_object$err_NMSE)

missForest_object$err_NMSE %>% 
  mutate(iteration = row_number()) %>% 
  pivot_longer(cols = all_of(col_names),
               values_to = "NMSE") %>% 
  ggplot(aes(iteration, NMSE, col = name)) +
  geom_point() +
  geom_line()



## -----------------------------------------------------------------------------
evaluate_imputation_error(missForest_object$ximp, iris_mis, iris)


