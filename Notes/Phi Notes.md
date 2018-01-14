# Notes on Gradient Boosting

## Data Preparation

1. How to handle imputation? Both academically (aka with papers) and technically (with packages like MICE)
2. Feature selection:
    1. Demographic information
    2. Order information
    3. Item information
    4. Additional information that we would like but can't add (e.g. online reviews, checkout process, etc.)
3. How does xgboost handle non-linear interactions? Can it create a non-linear decision boundary with being fed linear features? - apparently yes it can! since it's a tree
4. Do we need to do any type of feature selection? Does it natively do feature selection? - looks like it does!

## Model Generation

1. How does the model work? Combines simple classifiers - each round of boosting corrects error of prior model by using gradients (residuals) - Friedman (2001)
2. Can gradient accept different loss functions? Apparently yes (try this)
    1. Alternatively apply cutoff after probabilities have been assigned
3. What metaparameters need to be tuned? Find optimal learning rate, tree depth, number of iterations, keep others default
    1. Number of iterations
    2. Maximum depth of trees
    3. Shrinkage / learning rate (eta)
    4. Minimum loss reduction - regulariozation (gamma)
    5. Subspace ratio of columns
    6. Minimum sum of instance weight

## Model Evaluation

1. Do we need to cross-validate? If so, why? Does boosting natively do cross-validation? - looks like it can handle cross-validation
    - Remember to add notes on what exactly cross-validation does (reduce out-of-sample error)
2. [Cost-sensitive classification](https://mlr-org.github.io/mlr-tutorial/release/html/cost_sensitive_classif/index.html#class-dependent-misclassification-costs)

## Prediction

## Additional Notes

1. What is the motivation for boosting, what are the advantages / disadvantages etc?
    - SPEED! Since it combines many small, simple classifiers, the trees don't get too deep
2. How does it improve upon predictions? By reducing variance or bias?

## Resources

1. https://mlr-org.github.io/mlr-tutorial/release/html/index.html
2. https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
3. https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
