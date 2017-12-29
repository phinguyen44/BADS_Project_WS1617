# BADS Project
bads-ws1718-group21_ created by GitHub Classroom


![image](https://qph.ec.quoracdn.net/main-qimg-527cbeca6d5ab2127118ace7d469b087)

## Project Details

Next Steps for BADS Group Assignment

A) Final Data Cleaning
 - remove unplausible values (e.g. age above/below certain value)
 - impute missing values (median, mean or via ML)
    -> see notes from DAII (Claudia)


B) Feature Creation: Create additional, useful variables
  - Age
  - Delivery time 
  - Not returned (NA for delivery date)
  - cluster: brands/ sizes?
 -> see lecture on feature creation
 
 C) Exploratory Data analysis // parallel to B)
 - summary stats
 - find useful patterns in data 
 - mean return based on criteria
     item (brand, size)   customer (age, title, state)   delivery (time, season)
 
D) Compare known (training) and unknown data set
  - similarities & differences: what does this mean for our prediction?
  - do data set belong to same population? See lecture notes on this
  => the answer is probably yes

E) Model creation

- simple logistic regression (from individual assignment)
- neural network (make sure to standardize variables)
- Random forest (sensible to variables with too many levels) + bagging
 (or gradient boosted tree (usually slightly better than RF))

F) Model evaluation & comparison
- N-fold cross-validation -> calculate average AUC and plot
- ROC
- Last step: Try averaging results from all models or buid log model on all models 
 => check whether gives best results

G) Predictions
