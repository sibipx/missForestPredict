# missForest

missForestPredict is an adaptation of the missForest algorithm for prediction settings. It stores the initialization and imputation models and it can impute new (unseen) observations. 

Check the vignettes for instructions on how to use the package.

Functionality:

- uses ranger package for random forest models (fast imputation)

- saves initialization & support for custom initialization

- builds models for non-missing variables

- saves imputation models for all iterations

- imputes (predict) new observation(s)

- user can specify variables to be imputed and predictors for each variable using predictor_matrix and proportion_usable_cases

- error monitoring 

- convergence criteria: based on OOB or apparent performance (default OOB)

- convergence criteria: NMSE for both continuous and categorical variables (multiclass included)

- convergence criteria: custom weighting of the errors for each variable




