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
needs(tidyverse, caret, mlr, xgboost, mice, pROC, parallel, parallelMap)

# Load data
load("Data/BADS_WS1718_known_imp1.RData")
df.known <- read.csv("Data/BADS_WS1718_class.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")

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

# SPLIT DATA
set.seed(321)
idx.train <- createDataPartition(y = df.train$return, p = 0.8, list = FALSE)
tr <- df.train[idx.train, ]   # training set
ts <- df.train[-idx.train, ]  # test set

################################################################################
# BUILD MODEL

# xgboost accepts target variable separately
tr.label <- tr$return
ts.label <- ts$return
train    <- tr %>% select(-return)
test     <- ts %>% select(-return)

# one hot encoding - this is required for xgboost
train <- model.matrix(~ 0 + ., train)
test  <- model.matrix(~ 0 + ., test)

dtrain <- xgb.DMatrix(data = train, label = tr.label) 
dtest  <- xgb.DMatrix(data = test, label = ts.label)

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

xgbpred  <- predict(xgb1, dtest)
xgbpred1 <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix(xgbpred1, ts.label)
performance.met(ts.label, xgbpred)

# Variable importance plot
mat <- xgb.importance(feature_names = colnames(tr), model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:20]) 

# Check reliability plot
reliability.plot(act = ts.label, pred = xgbpred)

# Check results after platt scaling
plattpred  <- platt(act = ts.label, pred = xgbpred)
plattpred1 <- ifelse(plattpred > 0.5,1,0)
confusionMatrix(ts.label, plattpred1)
reliability.plot(act = ts.label, pred = plattpred)

################################################################################
# BUILD MODEL (part 2, with MLR)

traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)

# one hot encoding
traintask <- createDummyFeatures(obj = traintask) 
testtask  <- createDummyFeatures(obj = testtask)

# create learner
lrn <- makeLearner("classif.xgboost", predict.type = "prob")

params <- makeParamSet(
    makeDiscreteParam("booster", values = c("gbtree", "dart")), 
    makeDiscreteParam("gamma", values = c(0, 0.1, 0.2, 0.3)),
    makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
    makeDiscreteParam("nrounds", values = c(20, 50, 100)), 
    makeIntegerParam("max_depth", lower = 3L, upper = 10L), 
    makeNumericParam("min_child_weight", lower = 1L, upper = 10L), 
    makeNumericParam("subsample", lower = 0.5, upper = 1), 
    makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
    )

# cross-validation
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)

# random search
ctrl  <- makeTuneControlRandom(maxit = 10L)

parallelStartSocket(cpus = detectCores())

xgb.tuning <- tuneParams(learner    = lrn, 
                         task       = traintask, 
                         resampling = rdesc,
                         par.set    = params, 
                         control    = ctrl, 
                         measures   = acc,
                         show.info  = TRUE
                         )

parallelStop()

# Extract optimal parameter values after tuning 
xgb.tuning$x

lrn <- setHyperPars(lrn, par.vals = c(xgb.tuning$x, "verbose" = 0))

# train & predic model
xgmodel <- train(learner = lrn, task = traintask)
xgpred  <- predict(xgmodel, testtask)

# performance
confusionMatrix(xgpred$data$response, ts.label)

reliability.plot(act = ts.label, pred = xgpred$data$prob.1)
# Check results after platt scaling
plattpred  <- platt(act = ts.label, pred = xgpred$data$prob.1)
plattpred1 <- ifelse(plattpred > 0.5,1,0)
confusionMatrix(ts.label, plattpred1)
reliability.plot(act = ts.label, pred = plattpred)

################################################################################
# PREDICT
