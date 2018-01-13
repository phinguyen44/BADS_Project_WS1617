# Notes on Gradient Boosting

## Data Preparation

1. How to handle imputation? Both academically (aka with papers) and technically (with packages like MICE)
2. Feature selection:
    1. Demographic information
    2. Order information
    3. Item information
    4. Additional information that we would like but can't add (e.g. online reviews, checkout process, etc.)
3. How does xgboost handle non-linear interactions? Can it create a non-linear decision boundary with being fed linear features?
4. Do we need to do any type of feature selection? Does it natively do feature selection?

## Model Generation

1. How does the model work? Combines simple classifiers - each round of boosting corrects error of prior model by using gradients (residuals) - Friedman (2001)
2. Can gradient accept different loss functions? Apparently yes (try this)
    1. Alternatively apply cutoff after probabilities have been assigned
3. What metaparameters need to be tuned? Find optimal learning rate, tree depth, number of iterations, keep others default
    1. Number of iterations
    2. Maximum depth of trees
    3. Shrinkage / learning rate (eta)
    4. Minimum loss reduction (gamma)
    5. Subspace ratio of columns
    6. Minimum sum of instance weight

## Model Evaluation

1. Do we need to cross-validate? If so, why? Does boosting natively do cross-validation?
2. Log-loss? Brier score?

## Prediction

## Additional Notes

1. What is the motivation for boosting, what are the advantages / disadvantages etc?
2. How does it improve upon predictions? By reducing variance or bias?

## Resources

1. https://github.com/dmlc/xgboost/tree/master/demo
2. http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
3. http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html
