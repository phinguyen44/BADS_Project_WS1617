################################################################################
# Helpful-Models.R
#
################################################################################
# Description:
# BADS project - includes 5 different model implementations using the mlr
# package. CV is excluded and done separately.
# 
# logistic regression (glm, no regularization)
# decision trees (rpart)
# random forest (randomForest)
# gradient boosting (xgb)
# neural net (nnet) - single hidden layer neural network
# 
################################################################################

# LOGISTIC
lr.mod <- function(learner, traintask, testtask) {
    
    # make learner
    lr.model  <- makeLearner(learner, predict.type = "prob")
    
    start <- Sys.time()
    # train model
    lr      <- train(lr.model, traintask)
    lr.pred <- predict(lr, testtask)
    model   <- lr$learner.model
    
    # predict
    lr.yhat       <- lr.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = lr.yhat, model = model))
}

# DECISION TREE
dt.mod <- function(learner, traintask, testtask) {
    
    # make learner
    makeatree <- makeLearner(learner, predict.type = "prob")
    
    # cross-validation
    set_cv <- makeResampleDesc("CV",iters = 4L)
    
    # hyperparameters
    gs <- makeParamSet(
        makeIntegerParam("minsplit",lower = 10, upper = 30),
        makeIntegerParam("minbucket", lower = 5, upper = 30),
        makeNumericParam("cp", lower = 0.001, upper = 0.05)
    )
    
    # grid search
    # gscontrol <- makeTuneControlGrid()
    # random search
    gscontrol <- makeTuneControlRandom(maxit = 20L)
    
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
    t.tree    <- setHyperPars(makeatree, par.vals = stune$x)
    hyperpars <- stune$x
    
    # train model
    t.rpart <- train(t.tree, traintask)
    t.pred  <- predict(t.rpart, testtask)
    
    # predict
    t.yhat       <- t.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = t.yhat, pars = hyperpars))
    
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
    hyperpars <- rf_tune$x
    
    # train model
    rforest  <- train(rf.tree, traintask)
    rf.pred  <- predict(rforest, testtask)
    
    # predict
    rf.yhat       <- rf.pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    return(list(pred = rf.yhat, pars = hyperpars))
}

# XGB
xgb.mod <- function(learner, traintask, testtask) {
    
    # one hot encoding
    traintask <- createDummyFeatures(obj = traintask) 
    testtask  <- createDummyFeatures(obj = testtask)
    
    # make learner
    xg_set <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)
    
    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree", "dart")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 0.2)),
        makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
        makeDiscreteParam("nrounds", values = c(20, 50, 100)),
        makeDiscreteParam("subsample", values = 0.80),
        makeDiscreteParam("min_child_weight", values = 1),
        makeDiscreteParam("colsample_bytree", values = 0.8),
        makeIntegerParam("max_depth", lower = 3L, upper = 8L),
        makeNumericParam("lambda", lower = 0.55, upper = 0.60)
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
    hyperpars <- xg_tune$x
    
    xg_model <- train(xg_new, traintask)
    xg_pred  <- predict(xg_model, testtask)
    
    # predict
    xg.yhat       <- xg_pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = xg.yhat, pars = hyperpars))
    
}

# NNET
nn.mod <- function(learner, traintask, testtask) {
    
    nn <- makeLearner(learner, predict.type = "prob")
    
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)
    
    nn_par <- makeParamSet(
        makeDiscreteParam("size", values = seq(2, 8, by=1)),
        makeDiscreteParam("decay", values = seq(0, 0.1, by=0.01))
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
    hyperpars <- tune_nn$x
    
    nn_mod  <- train(final_nn, traintask)
    nn_pred <- predict(nn_mod, testtask)
    
    # predict
    nn.yhat       <- nn_pred$data$prob.1
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = nn.yhat, pars = hyperpars))
    
}