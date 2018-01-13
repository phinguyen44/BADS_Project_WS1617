################################################################################
# XGBoost.R
#
################################################################################
# Description:
# BADS project - predict using gradient boosting 
# (with platt scaling?)
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, caret, mlr, xgboost, mice, pROC, doParallel)

# Load data
load("Data/BADS_WS1718_known_imp1.RData")
df.known <- read.csv("Data/BADS_WS1718_class.csv")

################################################################################
# FEATURES

# select variables
df.train <- dat.input1 %>% 
    select(age, user_state, user_title,
           deliver.time, order_year, order_month, weekday, no.return,
           item_price,
           return)

# convert to factor
df.train <- df.train %>% 
    mutate(order_year = as.factor(order_year),
           weekday    = as.factor(weekday))

################################################################################
# BUILD MODEL

# SPLIT DATA
set.seed(321)
idx.train <- createDataPartition(y = df.train$return, p = 0.8, list = FALSE)
tr <- df.train[idx.train, ]   # training set
ts <- df.train[-idx.train, ]  # test set

# xgboost accepts target variable separately
tr.label <- tr$return
ts.label <- ts$return
tr       <- tr %>% select(-return)
ts       <- ts %>% select(-return)

# one hot encoding - this is required for xgboost
tr <- model.matrix(~ 0 + ., tr)
ts <- model.matrix(~ 0 + ., ts)

dtrain <- xgb.DMatrix(data = tr,label = tr.label) 
dtest  <- xgb.DMatrix(data = ts,label = ts.label)

# PARAMETERS
params <- list(booster          = "gbtree", # gbtree or dart
               objective        = "binary:logistic", # fixed, don't change
               eta              = 0.3, # learning rate, between 0.1 and 0.3
               gamma            = 0,   # regularization
               max_depth        = 6,   # max depth of tree
               min_child_weight = 1,   # ???
               subsample        = 1,   # ???
               colsample_bytree = 1    # ???
               )   

# Run model with cross-validation
xgbcv <- xgb.cv(params                 = params, 
                data                   = dtrain, 
                nrounds                = 100, 
                nfold                  = 5, # dataset into how many subsamples
                showsd                 = T, 
                stratified             = T, 
                print_every_n          = 10, 
                early_stoppping_rounds = 20, 
                maximize               = F)

# Find best iteration
niter = which(xgbcv$evaluation_log$train_error_mean == 
                  min(xgbcv$evaluation_log$train_error_mean))

# First pass
xgb1 <- xgb.train(params                = params, 
                  data                  = dtrain, 
                  nrounds               = niter, 
                  watchlist             = list(val=dtest,train=dtrain), 
                  print_every_n         = 10,
                  early_stopping_rounds = 10, 
                  maximize              = F , 
                  eval_metric           = "error")

################################################################################
# EVALUATE

xgbpred <- predict(xgb1, dtest)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix(xgbpred, ts.label)

# Variable importance plot
mat <- xgb.importance(feature_names = colnames(tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:20]) 

# TODO: Check reliability plot

################################################################################
# PREDICT
