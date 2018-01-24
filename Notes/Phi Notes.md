# Notes from PHIKACHU

## Data Preparation

1. How to handle imputation? Both academically (aka with papers) and technically (with packages like MICE)
2. In the future do something like mahalanobis transformation.
3. How does xgboost (or all the models) handle non-linear interactions? Can it create a non-linear decision boundary with being fed linear features? - apparently yes it can! since it's a tree, automatically detects non-linear feature interactions (STILL NEED PAPER FOR THIS)
    - Note that omitting the `no.return` variable and only including it in post-processing reduces the overall performance. why? maybe because of interactions?
4. Use [WOE](https://stats.stackexchange.com/questions/189568/replacing-variables-by-woe-weight-of-evidence-in-logistic-regression/229039) - includes advantages and disadvantages
    - Don't forget to 0 out cases in unknown set if they don't appear in known set
    - How do we prevent overfitting (bias) with WOE? Use cross-validation then perform WOE on the variables
5. Wrapper and filter for feature selection
    - within the inner loop, use a wrapper
    - add plots for filtering and using your functions
    - wrapper uses the performance of a learning algorithm to assess usefulness of a feature set. learner is trained repeatedly on different feature subsets and the subset that leads to best performance is chosen
    - advantage of wrapper is the ability to check interactions; however it is incredibly computationally intensive

## Model Generation

1. How does the gradient boosting work? Combines simple classifiers - each round of boosting corrects error of prior model by using gradients (residuals) - Friedman (2001)
2. Can gradient accept different loss functions? Apparently yes (can accept any differentiable loss function)
    1. Alternatively apply cutoff after probabilities have been assigned
3. Hyperparameters for our models:
    1. xgboost: (first 2 control complexity to prevent overfitting, next 2 add randomness to make training more robust to noise)
        - max_depth: depth of tree. higher values increase complexity / overfitting
        - gamma: minimum loss reduction required to further partition a tree. (regularization) higher values decrease model complexity
        - eta: step size shrinkage used in update to prevents overfitting (shrinks feature weights at each step)
        - nrounds: number of rounds for boosting
        - booster: gbtree or dart. read documentation on 'dart'
        - lambda: l2 regularization. higher numbers reduces overfitting
    2. nnet:
        - size: number of units in hidden layer (only one hidden layer)
        - decay: regularization parameter (good values?)
    3. randomForest:
        - ntree: number of trees to grow. should not be too low, to ensure every input row gets predicted a few times.
        - mtry: number of variables randomly sampled. default is sqrt(#vars)
        - nodesize: min size of terminal nodes. the higher the number the smaller the tree (hence faster)
4. Should we apply some heterogeneous ensembling? What are the benefits (Carauna et al)
    - research shows that an effective ensemble includes models that are highly correct and make errors on different parts of input space (what are drawbacks of each model?) (Opitz)
    - varying feature subsets used by each member of ensemble should promote this necessary diversity (Opitz) - features should promote _disagreement_ between models
    - efficacy of a set of features depends on the learning algorithm itself (each learner may have different feature set) (Opitz)
5. What is our approach to ensembling:
    - hill climb (with replacement) Caruana
    - each output must be calibrated first
5. Platt scaling to calibrate -> smooths out so they better match posterior probabilities - also has an effect of reducing false positives (only in this case, by shifting predictions downward slightly)
    - Calibration should be done on a separately trained set, then the output of the training set should be passed through the calibrated model (Niculescu)
    - Add reliability plots to show how each output changed
    - See notes on [posterior probabilities](https://en.wikipedia.org/wiki/Posterior_probability) on wikipedia

## Model Evaluation

1. Cross-validation does one of two things: 1) model selection (by finding best hyperparameters OR averaging results) 2) error estimation of a model (measure of out-of-sample accuracy)
2. Benchmark experiments - show performance of cross-validation across resampling iterations. Plot as a ggplot with error bars
3. Create plots of classifier performance as a function of decision threshold for binary classification: `df = generateThreshVsPerfData(list(lda = pred1, ksvm = pred2), measures = list(fpr, fnr, mmce))`
    - From there we can also plot ROC curves: `plotROCCurves(df)`
    - Maybe instead of mmce, we do results of our cost matrix
    - Plot reliability plots
4. [Cost-sensitive classification](https://mlr-org.github.io/mlr-tutorial/release/html/cost_sensitive_classif/index.html#class-dependent-misclassification-costs)
    - Could each model have it's own cutoff? i.e. minimize cost at individual model, then minimize at total?
    - In our cost matrix, a false positive is an opportunity cost, but a False Negative is actual cost (show table)
    - Our problem is one of example-dependent misclassification costs - that is, costs are associated at the individual case-level, on the price of the item in question.
    - We can either use an ROCIV approach (as described by Fawcett 2006) and find the optimal cutoff point, or use a naive bayes approach for each individual (extension of approach in Elkan 2001)

## Prediction

1. We should aim to focus on extreme cases (namely, we should only predict return if we're reasonably confident)

## Additional Notes

1. What is the motivation for boosting, what are the advantages / disadvantages etc?
    - SPEED! Since it combines many small, simple classifiers, the trees don't get too deep
    - "The most important factor behind the success of XGBoost is its scalability in all scenarios.  The system runs more than ten times faster than existing popular solutions on a single machine and scales to billions of examples in distributed or
    memory-limited settings." - Chen/Guestrin 2016
    - xgb vs gbm?
    - xgboost also has DART regularization
    - Advantage over trees: doesn't overfit, can include predictive power from mutiple, overlapping regions of the feature space (one tree only considers each additional feature inside a small region of input space)
    - Gradient boosting is easy to understand, isn't restricted to just using trees (can use any base model)
    - Each iteration is trained on on the cost reduction for each sample if the predicted value were to become one unit closer to the target value
2. How does it improve upon predictions? By reducing variance or bias?
    - Domingos 2000

## Resources

1. https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
2. https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
3. https://datascience.stackexchange.com/questions/13960/how-to-choose-a-classifier-after-cross-validation
4. https://stats.stackexchange.com/questions/65128/nested-cross-validation-for-model-selection
5. [Graphic on nested resampling](https://mlr-org.github.io/mlr-tutorial/release/html/nested_resampling/index.html)
