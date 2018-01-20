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
# rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, magrittr, purrr, infuser,
      caret, mlr, xgboost, gbm, rpart, e1071, MASS, nnet, 
      mice, pROC, parallel, parallelMap)

# Load data
load("Data/BADS_WS1718_known_imp1.RData")
load("Data/BADS_WS1718_known_feat.RData")
df.known <- read.csv("Data/BADS_WS1718_known.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")

################################################################################
# FEATURES

# select variables
df.train <- dat.input1 %>%
    dplyr::select(age, user_state, user_title,
                  deliver.time, order_year, order_month, weekday, no.return,
                  item_price,
                  return)

# convert to factor
df.train <- df.train %>%
    dplyr::mutate(order_year = as.factor(order_year),
                  weekday    = as.factor(weekday))

# other one
df.train2 <- dat.input2 %>%
    dplyr::select(age, user_state, user_title, user_purchase_num, #user_id_WOE,
                  deliver.time, order_year, order_month, weekday, no.return,
                  order_size, 
                  #item_id_WOE, item_color_WOE, item_size_WOE,
                  order_same_item, order_same_cs, order_same_cb, order_same_bs,
                  item_price,
                  return)

################################################################################
# BUILD MODEL

# SPLIT DATA
set.seed(321)
idx.train <- createDataPartition(y = df.train$return, p = 0.8, list = FALSE)
tr <- df.train[idx.train, ]   # training set
ts <- df.train[-idx.train, ]  # test set

tr.label <- tr$return
ts.label <- ts$return

## Get for post-processing later
# post.process <- ts$no.return
# tr <- tr %>% dplyr::select(-no.return)
# ts <- ts %>% dplyr::select(-no.return)

traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)

# SPLIT DATA # PART 2
idx.train2 <- createDataPartition(y = df.train2$return, p = 0.8, list = FALSE)
tr2 <- df.train2[idx.train2, ]   # training set
ts2 <- df.train2[-idx.train2, ]  # test set

tr.label2 <- tr2$return
ts.label2 <- ts2$return

# ## Get for post-processing later
# post.process2 <- ts2$no.return
# tr2 <- tr2 %>% dplyr::select(-no.return)
# ts2 <- ts2 %>% dplyr::select(-no.return)

traintask2 <- makeClassifTask(data = tr2, target = "return", positive = 1)
testtask2  <- makeClassifTask(data = ts2, target = "return", positive = 1)

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
    cat(paste0(learner, " run time: "))
    print(runtime)
    
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
    # gscontrol <- makeTuneControlGrid()
    # random search
    gscontrol <- makeTuneControlRandom(maxit = 30L)
    
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
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(t.yhat)

}

# RANDOM FOREST
rf.mod <- function(learner, traintask, testtask) {
    
    # make Learner
    rf <- makeLearner(learner, predict.type = "prob", 
                      par.vals = list(ntree = 200, mtry = 3, importance = TRUE))
    
    # hyperparameters
    rf_param <- makeParamSet(
        makeIntegerParam("ntree",lower = 50, upper = 200),
        makeIntegerParam("mtry", lower = 3, upper = 10),
        makeIntegerParam("nodesize", lower = 10, upper = 40)
    )
    
    # tune parameters (random rather than grid search faster)
    rancontrol <- makeTuneControlRandom(maxit = 30L)
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
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(rf.yhat)
}

# XGB
xgb.mod <- function(learner, traintask, testtask) {
    
    # one hot encoding
    traintask <- createDummyFeatures(obj = traintask) 
    testtask  <- createDummyFeatures(obj = testtask)
    
    # make learner
    xg_set <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 50L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree", "dart")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 0.2, 0.3)),
        makeDiscreteParam("nrounds", values = c(20, 50, 100)),
        makeIntegerParam("max_depth", lower = 3L, upper = 20L),
        makeNumericParam("lambda", lower = 0.55,upper = 0.60),
        makeNumericParam("eta", lower = 0.001, upper = 0.5),
        makeNumericParam("subsample", lower = 0.10, upper = 0.80),
        makeNumericParam("min_child_weight", lower = 1L, upper = 5L),
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
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(xg.yhat)
    
}

# NNET
nn.mod <- function(learner, traintask, testtask) {
    
    nn <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 3L)
    
    nn_par <- makeParamSet(
        makeDiscreteParam("size", values = seq(1, 8, by=1)),
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
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(nn.yhat)
    
}

## LIST OF FUNCTIONS
learners <- list(lr = "classif.logreg",
                 nn  = "classif.nnet",
                 xgb = "classif.xgboost",
                 dt = "classif.rpart",
                 rf = "classif.randomForest"
                 )
mods <- list(lr = lr.mod,
             nn  = nn.mod,
             xgb = xgb.mod,
             dt = dt.mod,
             rf = rf.mod
             )

yhat   <- map2(mods, learners, function(f, x) f(x, traintask, testtask))
yhat.r <- lapply(yhat, round)

yhat.new   <- map2(mods, learners, function(f, x) f(x, traintask2, testtask2))
yhat.r.new <- lapply(yhat.new, round)

################################################################################
# POST-PROCESSING

# TODO: CHECK the post-processing

# Post-processing with no.returns at the end
# idx  <- which(post.process==1)
# idx2 <- which(post.process2==1)
# 
# yhat.r.post <- yhat.r
# for (i in 1:length(yhat.r)) {
#     yhat.r.post[[i]][idx] <- 0
# }
# 
# yhat.r.post.new <- yhat.r.new
# for (i in 1:length(yhat.r)) {
#     yhat.r.post.new[[i]][idx2] <- 0
# }

# get confusion matrices for all:
# 1) old model, old model + post processing
# 2) new model, new model + post processing

cMat          <- lapply(yhat.r, function(x) confusionMatrix(x, ts.label, positive = "1"))
# cMat.post     <- lapply(yhat.r.post, function(x) confusionMatrix(x, ts.label, positive = "1"))
cMat.new      <- lapply(yhat.r.new, function(x) confusionMatrix(x, ts.label2, positive = "1"))
# cMat.new.post <- lapply(yhat.r.post.new, function(x) confusionMatrix(x, ts.label2, positive = "1"))

################################################################################
# EVALUATE

auc    <- lapply(yhat.r, function(x) measureAUC(truth = ts.label, 
                                                probabilities = x, 
                                                negative = 0, 
                                                positive = 1))
cMat   <- lapply(yhat.r, function(x) confusionMatrix(x, ts.label, positive = "1"))

# PLOT RELIABILITY PLOT. SELECT ONE OF THE MODELS IN the PRED arg
reliability.plot(act = ts.label, pred = yhat$lr)

yhat.r.name <- infuse("Data/Predictions - Phi/run_{{rundate}}_yhat.Rdata",
                      rundate = strftime(Sys.Date(), "%Y%m%d"))
cMat.name   <- infuse("Data/Predictions - Phi/run_{{rundate}}_cMat.Rdata",
                      rundate = strftime(Sys.Date(), "%Y%m%d"))

save(yhat.r, file = yhat.r.name)
save(cMat, file = cMat.name)

################################################################################
## NOTE: STUFF BELOW HERE IS OPTIONAL, HENCE CODE IS UGLY

# must feed in each pred individually

# Check results after platt scaling
# plattpred  <- platt(act = ts.label, pred = yhat$xgb)
# plattpred1 <- ifelse(plattpred > 0.5,1,0)
# confusionMatrix(ts.label, plattpred1, positive = "1")
# 
# reliability.plot(act = ts.label, pred = plattpred)
# 
# x = data.frame(actual = ts.label, pred = yhat$rf, plattpred = plattpred)
# 
# p1 <- ggplot(data = x, aes(pred, color = as.factor(actual))) +
#     geom_density(size = 1) +
#     geom_vline(aes(xintercept = 0.5), color = "blue") + 
#     labs(title = "Training Set Predicted Score") + 
#     theme_minimal() +
#     theme(panel.grid.minor = element_blank()) + 
#     theme(panel.grid.major.x = element_blank())
# p1
# 
# p2 <- ggplot(data = x, aes(plattpred, color = as.factor(actual))) +
#     geom_density(size = 1) +
#     geom_vline(aes(xintercept = 0.5), color = "blue") + 
#     labs(title = "Training Set Predicted Score, After Platt Scaling") + 
#     theme_minimal() +
#     theme(panel.grid.minor = element_blank()) + 
#     theme(panel.grid.major.x = element_blank())
# p2
