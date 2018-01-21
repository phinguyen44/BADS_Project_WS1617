################################################################################
# Helpful-Models.R
#
################################################################################
# Description:
# BADS project - includes 5 different model implementations using the mlr
# package. inner loop CV also included for hyperparameter tuning
# 
# calib.part() - create data partitions for calibration
# calib.mod() - run prediction through a calibrator
#
# logistic regression (glm, no regularization)
# decision trees (rpart)
# random forest (randomForest)
# gradient boosting (xgb)
# neural net (nnet) - single hidden layer neural network
#
# 
################################################################################

# TODO: FOR EACH MODEL:
# 1) add a feature selection wrapper (either filter or wrapper or both)

# TODO: finish calibration for rest of models!

calib.part <- function(tr) {
    tr.p <- tr
    
    #  SPLIT DATA FOR PLATT SCALING
    idx.p <- createDataPartition(y = tr.p$return, p = 0.8, list = FALSE)
    
    tr <- tr.p[idx.p, ]  # training set with platt
    cs <- tr.p[-idx.p, ] # calib set
    
    tr.label <- tr$return
    cs.label <- cs$return
    
    return(list(tr = tr, cs = cs, tr.label = tr.label, cs.label = cs.label))
}

calib.mod <- function(lr, calibtask) {
    
    # predict on calib set
    calib.pred <- predict(lr, calibtask)
    
    # train logreg on calib set
    clearner <- makeLearner("classif.logreg", predict.type = "prob")
    calib.df <- data.frame(y = cs.label, x = calib.pred$data$prob.1)
    ctask    <- makeClassifTask(data = calib.df, target = "y", positive = 1)
    calib.m  <- train(clearner, ctask)
    
    # pass test set prediction through calibrated model
    test.pred <- data.frame(y = lr.pred$data$truth, x = lr.pred$data$prob.1)
    task <- makeClassifTask(data = test.pred, target = "y", positive = 1)
    lr.pred.calib <- predict(calib.m, task)
    
    return(lr.pred.calib)
}

# LOGISTIC
lr.mod <- function(learner, tr, ts, calib = FALSE) {
    
    # make learner
    lr.model  <- makeLearner(learner, predict.type = "prob")
    
    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        calibtask <- makeClassifTask(data = cs, target = "return", positive = 1)
    }
    
    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
    testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)
    
    start <- Sys.time()
    # train model
    lr      <- train(lr.model, traintask)
    # predict
    lr.pred <- predict(lr, testtask)

    # pass prediction through calibrated model
    if (calib == TRUE) lr.pred.calib <- calib.mod(lr, calibtask)
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = lr.pred, pred.calib = lr.pred.calib))
}

# DECISION TREE
dt.mod <- function(learner, tr, ts, calib = FALSE) {
    
    # make learner
    makeatree <- makeLearner(learner, predict.type = "prob")
    
    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        calibtask <- makeClassifTask(data = cs, target = "return", positive = 1)
    }
    
    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
    testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)
    
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
    
    # predict
    t.pred  <- predict(t.rpart, testtask)
    
    # pass prediction through calibrated model
    if (calib == TRUE) t.pred.calib <- calib.mod(t.rpart, calibtask)
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = t.pred, pred.calib = t.pred.calib, pars = hyperpars))
    
}

# RANDOM FOREST
rf.mod <- function(learner, tr, ts, calib = FALSE) {
    
    # make Learner
    rf <- makeLearner(learner, predict.type = "prob", 
                      par.vals = list(ntree = 200, mtry = 3, importance = TRUE))
    
    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        calibtask <- makeClassifTask(data = cs, target = "return", positive = 1)
    }
    
    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
    testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)
    
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
    
    # predict
    rf.pred  <- predict(rforest, testtask)
    
    # pass prediction through calibrated model
    if (calib == TRUE) rf.pred.calib <- calib.mod(rforest, calibtask)
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = rf.pred, pred.calib = rf.pred.calib, pars = hyperpars))
}

# XGB
xgb.mod <- function(learner, tr, ts, calib = FALSE) {
    
    # make learner
    xg_set <- makeLearner(learner, predict.type = "prob")
    
    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        calibtask <- makeClassifTask(data = cs, target = "return", positive = 1)
        calibtask <- createDummyFeatures(obj = calibtask)
    }
    
    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
    testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)
    
    # one hot encoding
    traintask <- createDummyFeatures(obj = traintask) 
    testtask  <- createDummyFeatures(obj = testtask)
    
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)
    
    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree", "dart")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 0.2)),
        makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
        makeDiscreteParam("nrounds", values = c(50, 100, 200)),
        makeDiscreteParam("lambda", values = seq(0.3, 0.6, by=0.05)),
        makeIntegerParam("max_depth", lower = 3L, upper = 8L)
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
    
    # predict
    xg_pred  <- predict(xg_model, testtask)
    
    # pass prediction through calibrated model
    if (calib == TRUE) xg.pred.calib <- calib.mod(xg_model, calibtask)
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = xg_pred, pred.calib = xg.pred.calib, pars = hyperpars))
}

# NNET
nn.mod <- function(learner, tr, ts, calib = FALSE) {
    
    nn <- makeLearner(learner, predict.type = "prob")
    
    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        calibtask <- makeClassifTask(data = cs, target = "return", positive = 1)
    }
    
    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)
    testtask  <- makeClassifTask(data = ts, target = "return", positive = 1)
    
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)
    
    nn_par <- makeParamSet(
        makeDiscreteParam("size", values = seq(2, 8, by=1)),
        makeDiscreteParam("decay", values = seq(0, 0.5, by=0.1))
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
    
    # predict
    nn_pred <- predict(nn_mod, testtask)
    
    # pass prediction through calibrated model
    if (calib == TRUE) nn.pred.calib <- calib.mod(nn_mod, calibtask)
    
    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)
    
    return(list(pred = nn_pred, pred.calib = nn.pred.calib, pars = hyperpars))
    
}
