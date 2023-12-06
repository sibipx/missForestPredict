# missForest

missForestPredict is an adaptation of the missForest algorithm for prediction settings. It stores the initialization and imputation models and it can impute new (unseen) observations. 

It uses a single convergence criterion for continuous and categorical variables, based on the out-of-bag error.

It suitable for large and high dimensional datasets, as it s based on the ranger R package, which provides a fast C++ implementation of random forests.

Check the vignettes for instructions on how to use the package.

Adaptations:

- use ranger iso randomForest

- save initialization & support for custom initialization

- builds models for non-missing variables

- save models

- impute (predict) new observation(s)

- convergence criteria: based on OOB or apparent performance

- convergence criteria: NMSE for both continuous and categorical variables (multiclass included)

- convergence criteria: custom weighting of the errors for each variable

- control variables to be imputed and predictors for each variable using predictor_matrix and proportion_usable_cases

- better error monitoring 


