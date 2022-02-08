# missForest

missForestPredict is an adaptation of the missForest algorithm for prediction settings. It stores the initialization and imputation models and it can impute new (unseen) observations. 

It uses a single convergence criteria for continuous and categorical variables, based on the out-of-bag error.

It suitable for large and high dimensional datasets, as it s based on the ranger R package, which provides a fast C++ implementation of  random forests.

Changes done (for my record):

- use ranger iso randomForest

- save initialization & support for custom initialization

- build models for non-missing variables

- save models

- impute (predict) new observation(s)

- convergence criteria: OOB iso apparent performance

- convergence criteria: NMSE for both continuous and categorical variables (multiclass included)

- convergence criteria: custom weighting of the errors for each variable

- better error monitoring 

## For the future...

## Contact us

Contact me by email: 
