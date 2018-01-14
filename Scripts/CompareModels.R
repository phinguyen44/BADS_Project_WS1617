################################################################################
# CompareModels.R
#
################################################################################
# Description:
# BADS project - quick compare of some basic models
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, magrittr, purrr,
      caret, mlr, xgboost, gbm, rpart, e1071, MASS,
      mice, pROC, parallel, parallelMap)

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

################################################################################
# BUILD MODEL

# SPLIT DATA
set.seed(321)
idx.train <- createDataPartition(y = df.train$return, p = 0.8, list = FALSE)
tr <- df.train[idx.train, ]   # training set
ts <- df.train[-idx.train, ]  # test set

tr.label <- tr$return
ts.label <- ts$return

traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)

## CREATE EACH MODEL

# LOGISTIC
lr.mod <- function(learner, traintask, testtask) {
    
    # make learner
    lr.model  <- makeLearner(learner, predict.type = "prob")
    
    start <- Sys.time()
    # train model
    lr      <- train(lr.model, traintask)
    lr.pred <- predict(lr, testtask)
    
    # predict
    lr.yhat       <- lr.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(lr.yhat)
}

# DECISION TREE
dt.mod <- function(learner, traintask, testtask) {
    
    # make learner
    makeatree <- makeLearner(learner, predict.type = "prob")
    
    # cross-validation
    set_cv <- makeResampleDesc("CV",iters = 3L)
    
    # hyperparameters
    gs <- makeParamSet(
        makeIntegerParam("minsplit",lower = 10, upper = 30),
        makeIntegerParam("minbucket", lower = 5, upper = 30),
        makeNumericParam("cp", lower = 0.001, upper = 0.05)
    )
    
    # grid search
    gscontrol <- makeTuneControlGrid()
    
    start <- Sys.time()
    
    parallelStartSocket(cpus = detectCores())
    stune <- tuneParams(learner    = makeatree, 
                        resampling = set_cv, 
                        task       = traintask, 
                        par.set    = gs, 
                        control    = gscontrol, 
                        measures   = acc)
    parallelStop()
    
    # use hyperparameters
    t.tree <- setHyperPars(makeatree, par.vals = stune$x)
    
    # train model
    t.rpart <- train(t.tree, traintask)
    t.pred  <- predict(t.rpart, testtask)
    
    # predict
    t.yhat       <- t.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(t.yhat)

}

# RANDOM FOREST
rf.mod <- function(learner, traintask, testtask) {
    
    # make Learner
    rf <- makeLearner(learner, predict.type = "prob", 
                      par.vals = list(ntree = 200, mtry = 3, importance = TRUE))
    
    # hyperparameters
    rf_param <- makeParamSet(
        makeIntegerParam("ntree",lower = 50, upper = 500),
        makeIntegerParam("mtry", lower = 3, upper = 10),
        makeIntegerParam("nodesize", lower = 10, upper = 50)
    )
    
    # tune parameters (random rather than grid search faster)
    rancontrol <- makeTuneControlRandom(maxit = 20L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    start <- Sys.time()
    
    parallelStartSocket(cpus = detectCores())
    rf_tune    <- tuneParams(learner    = rf, 
                             resampling = set_cv, 
                             task       = traintask, 
                             par.set    = rf_param, 
                             control    = rancontrol, 
                             measures   = acc)
    parallelStop()
    
    # set hyperparameters
    rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)
    
    # train model
    rforest  <- train(rf.tree, traintask)
    rf.pred  <- predict(rforest, testtask)
    
    # predict
    rf.yhat       <- rf.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(rf.yhat)
}

# GBM
gbm.mod <- function(learner, traintask, testtask) {
    
    # make learner
    g.gbm <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 50L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    gbm_par <- makeParamSet(
        makeDiscreteParam("distribution", values = "bernoulli"),
        makeIntegerParam("n.trees", lower = 100, upper = 1000), 
        makeIntegerParam("interaction.depth", lower = 2, upper = 10), 
        makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
        makeNumericParam("shrinkage",lower = 0.01, upper = 1)
    )
    
    start <- Sys.time()
    
    parallelStartSocket(cpus = detectCores())
    tune_gbm <- tuneParams(learner    = g.gbm, 
                           task       = traintask,
                           resampling = set_cv,
                           measures   = acc,
                           par.set    = gbm_par,
                           control    = rancontrol
                           )
    parallelStop()
    
    final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)
    
    to.gbm <- train(final_gbm, traintask)
    pr.gbm <- predict(to.gbm, testtask)
    
    # predict
    gbm.yhat       <- pr.gbm$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(gbm.yhat)
    
}

# XGB
xgb.mod <- function(learner, traintask, testtask) {
    
    # one hot encoding
    traintask <- createDummyFeatures(obj = traintask) 
    testtask  <- createDummyFeatures(obj = testtask)
    
    # make learner
    xg_set <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 100L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree", "dart")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 0.2, 0.3)),
        makeDiscreteParam("nrounds", values = c(20, 50, 100)),
        makeIntegerParam("max_depth", lower = 3L, upper = 20L),
        makeNumericParam("lambda", lower = 0.55,upper = 0.60),
        makeNumericParam("eta", lower = 0.001, upper = 0.5),
        makeNumericParam("subsample", lower = 0.10, upper = 0.80),
        makeNumericParam("min_child_weight", lower = 1:, upper = 5L),
        makeNumericParam("colsample_bytree", lower = 0.2, upper = 0.8)
    )

    start <- Sys.time()
    
    parallelStartSocket(cpus = detectCores())
    xg_tune <- tuneParams(learner     = xg_set, 
                           task       = traintask,
                           resampling = set_cv,
                           measures   = acc,
                           par.set    = xg_ps,
                           control    = rancontrol
    )
    parallelStop()
    
    xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)
    
    xg_model <- train(xg_new, traintask)
    xg_pred  <- predict(xg_model, testtask)
    
    # predict
    xg.yhat       <- xg_pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(xg.yhat)
    
}

# NNET
nn.mod <- function(learner, traintask, testtask) {
    
    nn <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 50L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    nn_par <- makeParamSet(
        makeDiscreteParam("size", values = seq(1, 10, by=1)),
        makeDiscreteParam("decay", values = seq(0, 0.1, by=0.005))
    )
    
    start <- Sys.time()
    
    parallelStartSocket(cpus = detectCores())
    tune_nn <- tuneParams(learner    = nn, 
                          task       = traintask,
                          resampling = set_cv,
                          measures   = acc,
                          par.set    = nn_par,
                          control    = rancontrol
    )
    parallelStop()
    
    final_nn <- setHyperPars(learner = nn, par.vals = tune_nn$x)
    
    nn_mod  <- train(final_nn, traintask)
    nn_pred <- predict(nn_mod, testtask)
    
    # predict
    nn.yhat       <- nn_pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: ", round(runtime, 2), " secs"))
    
    return(nn.yhat)
    
}

## LIST OF FUNCTIONS
learners <- list(lr = "classif.logreg",
                 nn  = "classif.nnet",
                 gbm = "classif.gbm",
                 xgb = "classif.xgb",
                 dt = "classif.rpart",
                 rf = "classif.randomForest"
                 )
mods <- list(lr = lr.mod,
             nn  = nn.mod,
             gbm = gbm.mod,
             xgb = xgb.mod,
             dt = dt.mod,
             rf = rf.mod
             )

yhat <- map2(mods, learners, function(f, x) f(x, traintask, testtask))

################################################################################
# EVALUATE

yhat.r <- lapply(yhat, round)
auc    <- lapply(yhat.r, function(x) measureAUC(truth = ts.label, 
                                                probabilities = x, 
                                                negative = 0, 
                                                positive = 1))
cMat   <- lapply(yhat.r, function(x) confusionMatrix(x, ts.label))

reliability.plot(act = ts.label, pred = yhat$rf)

# Check results after platt scaling
plattpred  <- platt(act = ts.label, pred = yhat$rf)
plattpred1 <- ifelse(plattpred > 0.5,1,0)
confusionMatrix(ts.label, plattpred1)

reliability.plot(act = ts.label, pred = plattpred)

# PLATT SCALING APPEARS TO HAVE AN EFFECT OF DECREASING FPR

# TODO: STORE FINAL RESULTS IN RDATA FILE & RUN TIME
