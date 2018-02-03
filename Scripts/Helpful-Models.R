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
# ensembler() - ensembles models together using majority vote approach
# ensembler.final() - same as above, just used for final model
# to.numeric() - helper function used to convert mlr output to numeric
#
################################################################################

################################################################################
# Calibration

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

calib.mod <- function(mod, pred, cs, cs.label) {

    # predict on calib set
    calib.pred <- predict(mod, newdata = cs)

    # train logreg on calib set
    clearner <- makeLearner("classif.logreg", predict.type = "prob")
    calib.df <- data.frame(y = cs.label, x = calib.pred$data$prob.1)
    ctask    <- makeClassifTask(data = calib.df, target = "y", positive = 1)
    calib.m  <- train(clearner, ctask)

    # pass test set prediction through calibrated model
    test.pred     <- data.frame(y = pred$data$truth, x = pred$data$prob.1)
    lr.pred.calib <- predict(calib.m, newdata = test.pred)

    return(lr.pred.calib)
}

################################################################################
# Classifiers

# LOGISTIC
lr.mod <- function(learner, tr, ts, calib = FALSE) {

    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
    }

    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)

    start <- Sys.time()

    # make learner
    lr.model <- makeLearner(learner, predict.type = "prob")

    # train model
    lr      <- mlr::train(lr.model, traintask)

    # predict
    lr.pred <- predict(lr, newdata = ts)
    model   <- lr$learner.model

    # pass prediction through calibrated model
    if (calib == TRUE) {
        lr.pred.calib <- calib.mod(lr, lr.pred, cs, cs.label)
    }

    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    output <- list(pred = lr.pred)
    if (calib == TRUE) output[['pred.calib']] <- lr.pred.calib

    return(output)
}

# DECISION TREE
dt.mod <- function(learner, tr, ts, calib = FALSE) {

    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
    }

    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)

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
                        measures   = mlr::auc)
    parallelStop()

    # use hyperparameters
    t.tree    <- setHyperPars(makeatree, par.vals = stune$x)
    hyperpars <- stune$x

    # train model
    t.rpart <- mlr::train(t.tree, traintask)

    # predict
    t.pred  <- predict(t.rpart, newdata = ts)

    # pass prediction through calibrated model
    if (calib == TRUE) {
        t.pred.calib <- calib.mod(t.rpart, t.pred, cs, cs.label)
    }

    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    output <- list(pred = t.pred, pars = hyperpars)
    if (calib == TRUE) output[['pred.calib']] <- t.pred.calib

    return(output)

}

# RANDOM FOREST
rf.mod <- function(learner, tr, ts, calib = FALSE) {

    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
    }

    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)

    # make Learner
    rf <- makeLearner(learner, predict.type = "prob",
                      par.vals = list(ntree = 200, mtry = 3, importance = TRUE))

    # hyperparameters
    rf_param <- makeParamSet(
        makeDiscreteParam("ntree", values = seq(100, 300, by=20)),
        makeIntegerParam("nodesize", lower = 10, upper = 50),
        makeIntegerParam("mtry", lower = 5, upper = 9)
    )

    # tune parameters (random rather than grid search faster)
    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)

    start <- Sys.time()

    parallelStartSocket(cpus = detectCores())
    rf_tune    <- tuneParams(learner    = rf,
                             resampling = set_cv,
                             task       = traintask,
                             par.set    = rf_param,
                             control    = rancontrol,
                             measures   = mlr::auc)
    parallelStop()

    # set hyperparameters
    rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)
    hyperpars <- rf_tune$x

    # train model
    rforest  <- mlr::train(rf.tree, traintask)

    # predict
    rf.pred  <- predict(rforest, newdata = ts)

    # pass prediction through calibrated model
    if (calib == TRUE) {
        rf.pred.calib <- calib.mod(rforest, rf.pred, cs, cs.label)
    }

    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    output <- list(pred = rf.pred, pars = hyperpars)
    if (calib == TRUE) output[['pred.calib']] <- rf.pred.calib

    return(output)
}

# XGB
xgb.mod <- function(learner, tr, ts, calib = FALSE) {

    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs.label   <- calib.data$cs.label
        cs         <- createDummyFeatures(obj = calib.data$cs)
    }

    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)

    # one hot encoding
    traintask <- createDummyFeatures(obj = traintask)
    ts        <- createDummyFeatures(obj = ts)

    # make learner
    xg_set <- makeLearner(learner, predict.type = "prob")

    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)

    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 1, 10)),
        makeDiscreteParam("eta", values = c(0.001, 0.01, 0.1, 0.4)),
        makeDiscreteParam("nrounds", values = c(50, 100, 200, 400)),
        makeDiscreteParam("lambda", values = seq(0, 1, by=0.1)),
        makeIntegerParam("max_depth", lower = 2L, upper = 15L)
    )

    start <- Sys.time()

    parallelStartSocket(cpus = detectCores())
    xg_tune <- tuneParams(learner     = xg_set,
                          task       = traintask,
                          resampling = set_cv,
                          measures   = mlr::auc,
                          par.set    = xg_ps,
                          control    = rancontrol
    )
    parallelStop()

    xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)
    hyperpars <- xg_tune$x

    xg_model <- mlr::train(xg_new, traintask)

    # predict
    xg_pred  <- predict(xg_model, newdata = ts)

    # pass prediction through calibrated model
    if (calib == TRUE) {
        xg.pred.calib <- calib.mod(xg_model, xg_pred, cs, cs.label)
    }

    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    output <- list(pred = xg_pred, pars = hyperpars)
    if (calib == TRUE) output[['pred.calib']] <- xg.pred.calib

    return(output)
}

# NNET
nn.mod <- function(learner, tr, ts, calib = FALSE) {

    # split data for calibration
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        cs         <- calib.data$cs
        cs.label   <- calib.data$cs.label
    }

    traintask <- makeClassifTask(data = tr, target = "return", positive = 1)

    nn <- makeLearner(learner, predict.type = "prob")

    rancontrol <- makeTuneControlRandom(maxit = 30L)
    set_cv     <- makeResampleDesc("CV",iters = 4L)

    nn_par <- makeParamSet(
        makeDiscreteParam("size", values = seq(3, 8, by=1)),
        makeDiscreteParam("decay", values = c(1, 0.5, 0.1, 0.05, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7))
    )

    start <- Sys.time()

    parallelStartSocket(cpus = detectCores())
    tune_nn <- tuneParams(learner    = nn,
                          task       = traintask,
                          resampling = set_cv,
                          measures   = mlr::auc,
                          par.set    = nn_par,
                          control    = rancontrol
    )
    parallelStop()

    final_nn <- setHyperPars(learner = nn, par.vals = tune_nn$x)
    hyperpars <- tune_nn$x

    nn_mod  <- mlr::train(final_nn, traintask)

    # predict
    nn_pred <- predict(nn_mod, newdata = ts)

    # pass prediction through calibrated model
    if (calib == TRUE) {
        nn.pred.calib <- calib.mod(nn_mod, nn_pred, cs, cs.label)
    }

    end     <- Sys.time()
    runtime <- end - start
    cat(paste0(learner, " run time: "))
    print(runtime)

    output <- list(pred = nn_pred, pars = hyperpars)
    if (calib == TRUE) output[['pred.calib']] <- nn.pred.calib

    return(output)
}

################################################################################
# Ensemblers

# Build majority vote ensemble model, tie is broken by best model
ensembler <- function(allpreds, costlist) {
    
    # which is best model?
    best.mod <- which.max(costlist)
    
    # in case of tie, use prediction of best
    the.response <- data.frame(allpreds)
    the.means    <- rowMeans(the.response)
    m.idx        <- which(the.means == 0.5)
    
    # get final predictions
    final.results <- the.means
    final.results[m.idx] <- the.response[m.idx, best.mod]
    final.results <- round(final.results)
    
    return(final.results)
    
}

ensembler.final <- function(allpreds, costlist) {
    
    # which is best model?
    best.mod <- which.max(costlist)
    
    # in case of tie, use prediction of best
    the.response <- data.frame(sapply(allpreds, to.numeric))
    the.means    <- rowMeans(the.response)
    m.idx        <- which(the.means == 0.5)
    
    # get final predictions
    final.results <- the.means
    final.results[m.idx] <- the.response[m.idx, best.mod]
    final.results <- round(final.results)
    
    return(final.results)
    
}

# converts response to numeric
to.numeric <- function(pred.object) {
    
    response  <- pred.object$data$response
    predicted <- as.numeric(levels(response))[response]
    
    return(predicted)
    
}
