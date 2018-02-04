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
# random forest (randomForest)
# gradient boosting (xgb)
# neural net (nnet) - single hidden layer neural network
# 
# WOE() - self-made WOE function. outputs a vector
# z.scale() - standardize all numeric variables in df (except return!)
# woe.and.scale() - apply both previous functions to test and train sets
# 
# ensembler() - ensembles models together using majority vote approach
#
################################################################################

################################################################################
# Calibration

calib.part <- function(tr) {
    tr.p <- tr
    
    #  SPLIT DATA FOR PLATT SCALING
    idx.p <- createDataPartition(y = tr.p$return, p = 0.9, list = FALSE)

    tr <- tr.p[idx.p, ]  # training set with platt
    cs <- tr.p[-idx.p, ] # calib set

    tr.label <- tr$return
    cs.label <- cs$return

    return(list(tr       = tr,
                cs       = cs, 
                tr.label = tr.label, 
                cs.label = cs.label))
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
    test.pred     <- data.frame(x = pred$data$prob.1)
    lr.pred.calib <- predict(calib.m, newdata = test.pred)

    return(lr.pred.calib)
}

################################################################################
# Classifiers

# LOGISTIC
lr.mod <- function(learner, tr, ts, calib = FALSE, final = FALSE) {

    # make learner
    lr.model  <- makeLearner(learner, predict.type = "prob")

    start <- Sys.time()

    # calibration split data
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        csdf       <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        transformed.tf <- woe.and.scale(tr, csdf, final)
        cs             <- transformed.tf$testdf
    }
    
    # transform outer loop test set
    transformed  <- woe.and.scale(tr, ts, final)
    tr.transform <- transformed$traindf
    ts.transform <- transformed$testdf
    
    task.tf <- makeClassifTask(data     = tr.transform, 
                               target   = "return", 
                               positive = 1)
    
    # train model
    lr  <- mlr::train(lr.model, task.tf)
    
    # predict
    lr.pred <- predict(lr, newdata = ts.transform)
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

# RANDOM FOREST
rf.mod <- function(learner, tr, ts, calib = FALSE, final = FALSE) {

    ##### START OF INNER LOOP
    
    rf <- makeLearner(learner, predict.type = "prob",
                      par.vals = list(ntree = 400, mtry = 3, importance = TRUE))
    
    # resplit training data set manually
    part.ind <- createDataPartition(y = tr$return, p = 0.8, list = FALSE) 
    trdf     <- tr[part.ind, ]
    tsdf     <- tr[-part.ind, ] 
    
    # transform
    transformed <- woe.and.scale(trdf, tsdf, final)
    traindf     <- transformed$traindf
    testdf      <- transformed$testdf
    combdf      <- rbind(traindf, testdf)
    
    # create task and set control parameters (fixed holdout instance)
    traintask <- makeClassifTask(data = combdf, target = "return", positive = 1)
    
    # set control
    rancontrol <- makeTuneControlRandom(maxit = 20L)
    set_cv     <- makeFixedHoldoutInstance(
        train.inds = 1:nrow(traindf),
        test.inds  = (nrow(traindf)+1):nrow(combdf),
        size       = nrow(combdf)
    )

    # hyperparameters
    rf_param <- makeParamSet(
        makeDiscreteParam("ntree", seq(200, 400, by = 50)),
        makeIntegerParam("mtry", lower = 3, upper = 8),
        makeDiscreteParam("nodesize", seq(10, 40, by = 5))
    )

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
    
    ###### DONE WITH INNER LOOP, STARTING OUTER LOOP
    
    # calibration split data
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        csdf       <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        transformed.tf <- woe.and.scale(tr, csdf, final)
        cs             <- transformed.tf$testdf
    }
    
    # transform outer loop test set
    transformed  <- woe.and.scale(tr, ts, final)
    tr.transform <- transformed$traindf
    ts.transform <- transformed$testdf
    
    task.tf <- makeClassifTask(data     = tr.transform, 
                               target   = "return", 
                               positive = 1)
    
    # train model
    rforest  <- mlr::train(rf.tree, task.tf)
    
    # predict
    rf.pred <- predict(rforest, newdata = ts.transform)
    
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
xgb.mod <- function(learner, tr, ts, calib = FALSE, final = FALSE) {

    ##### START OF INNER LOOP
    
    xg_set <- makeLearner(learner, predict.type = "prob")
    
    # resplit training data set manually
    part.ind <- createDataPartition(y = tr$return, p = 0.8, list = FALSE) 
    trdf     <- tr[part.ind, ]
    tsdf     <- tr[-part.ind, ] 
    
    # transform
    transformed <- woe.and.scale(trdf, tsdf, final)
    traindf     <- transformed$traindf
    testdf      <- transformed$testdf
    combdf      <- rbind(traindf, testdf)
    
    # create task and set control parameters (fixed holdout instance)
    traintask <- makeClassifTask(data = combdf, target = "return", positive = 1)
    traintask <- createDummyFeatures(obj = traintask)     # one hot encoding
    
    # set control
    rancontrol <- makeTuneControlRandom(maxit = 20L)
    set_cv     <- makeFixedHoldoutInstance(
        train.inds = 1:nrow(traindf),
        test.inds  = (nrow(traindf)+1):nrow(combdf),
        size       = nrow(combdf)
    )
    
    xg_ps <- makeParamSet(
        makeDiscreteParam("booster", values = c("gbtree", "dart")),
        makeDiscreteParam("gamma", values = c(0, 0.1, 0.2)),
        makeDiscreteParam("eta", values = c(0.001, 0.01, 0.1, 0.3, 0.5)),
        makeDiscreteParam("nrounds", values = c(50, 100, 200)),
        makeDiscreteParam("lambda", values = seq(0, 0.6, by=0.1)),
        makeIntegerParam("max_depth", lower = 3L, upper = 8L)
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
    
    ###### DONE WITH INNER LOOP, STARTING OUTER LOOP

    # calibration split data
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        csdf       <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        transformed.tf <- woe.and.scale(tr, csdf, final)
        cs             <- transformed.tf$testdf
        cs             <- createDummyFeatures(obj = cs)
    }
    
    # transform outer loop test set
    transformed  <- woe.and.scale(tr, ts, final)
    tr.transform <- transformed$traindf
    ts.transform <- createDummyFeatures(obj = transformed$testdf)
    
    task.tf <- makeClassifTask(data     = tr.transform, 
                               target   = "return", 
                               positive = 1)
    task.tf <- createDummyFeatures(obj = task.tf)
    
    xg_model <- mlr::train(xg_new, task.tf)

    # predict
    xg_pred  <- predict(xg_model, newdata = ts.transform)

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
nn.mod <- function(learner, tr, ts, calib = FALSE, final = FALSE) {
    
    ##### START OF INNER LOOP
    
    nn <- makeLearner(learner, predict.type = "prob")
    
    # resplit training data set manually
    part.ind <- createDataPartition(y = tr$return, p = 0.8, list = FALSE) 
    trdf     <- tr[part.ind, ]
    tsdf     <- tr[-part.ind, ] 
    
    # transform
    transformed <- woe.and.scale(trdf, tsdf, final)
    traindf     <- transformed$traindf
    testdf      <- transformed$testdf
    combdf      <- rbind(traindf, testdf)
    
    # create task and set control parameters (fixed holdout instance)
    traintask <- makeClassifTask(data = combdf, target = "return", positive = 1)

    # set control
    rancontrol <- makeTuneControlRandom(maxit = 20L)
    set_cv     <- makeFixedHoldoutInstance(
        train.inds = 1:nrow(traindf),
        test.inds  = (nrow(traindf)+1):nrow(combdf),
        size       = nrow(combdf)
    )

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
    
    ###### DONE WITH INNER LOOP, STARTING OUTER LOOP
    
    # calibration split data
    if (calib == TRUE) {
        calib.data <- calib.part(tr)
        tr         <- calib.data$tr
        csdf       <- calib.data$cs
        cs.label   <- calib.data$cs.label
        
        transformed.tf <- woe.and.scale(tr, csdf, final)
        cs             <- transformed.tf$testdf
    }
    
    # transform outer loop test set
    transformed  <- woe.and.scale(tr, ts, final)
    tr.transform <- transformed$traindf
    ts.transform <- transformed$testdf
    
    task.tf <- makeClassifTask(data     = tr.transform, 
                               target   = "return", 
                               positive = 1)
    
    # train model
    nn_mod  <- mlr::train(final_nn, task.tf)

    # predict
    nn_pred <- predict(nn_mod, newdata = ts.transform)
    
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
# MODEL BUILDING

# WOE
WOE <- function(df, var) {
    
    require(dplyr)
    require(magrittr)
    
    df.new <- df %>% 
        group_by_(var) %>% 
        dplyr::summarize(Keep   = n() - sum(return),
                         Return = sum(return))
    
    ### Improve measure according to Zdravevski (2010)
    tot.keep <- sum(df.new$Keep)
    tot.ret  <- sum(df.new$Return)
    
    # Case 1: Keep = 0, Return = 0 -> WOE = 0
    # Case 2: Keep = 0, Return > 0 -> Keep = Keep + 1, 
    # Return = Return + tot.ret/tot.keep
    # Case 3: Keep > 0, Return = 0 -> Return = Return + 1, 
    # Keep = Keep + tot.keep/tot.ret
    # Otherwise, normal case.
    df.new$WOE <- with(df.new, 
        ifelse(Keep == 0 & Return == 0, 0,
        ifelse(Keep == 0 & Return > 0, log((Return*tot.keep + tot.ret)/tot.ret),
        ifelse(Keep > 0 & Return == 0, log(tot.keep/(Keep*tot.ret + tot.keep)),
                                       log((Return/tot.ret)/(Keep/tot.keep))))))
    
    # join data
    out <- left_join(df, df.new, var) %>% use_series(WOE)
    return(out)
    
}

z.scale <- function(df) {
    for (n in 1:ncol(df)) {
        if (is.numeric(df[, n]) & names(df)[n] != 'return') {
            middle  <- mean(df[, n])
            stdv    <- sd(df[, n])
            df[, n] <- (df[, n] - middle) / stdv
        }
    }
    return(data.frame(df))
}

woe.and.scale <- function(traindf, testdf, final = FALSE) {
    # standardize
    traindf <- z.scale(traindf)
    testdf <- z.scale(testdf)
    
    # add in WOE variables
    traindf$WOE.user_id     <- WOE(traindf, "user_id")
    traindf$WOE.item_id     <- WOE(traindf, "item_id")
    traindf$WOE.item_size   <- WOE(traindf, "item_size")
    traindf$WOE.brand_id    <- WOE(traindf, "brand_id")
    traindf$WOE.basket.size <- WOE(traindf, "basket.size")
    
    WOE.user_id <- traindf %>%
        dplyr::select(user_id, WOE.user_id) %>% distinct
    WOE.item_id <- traindf %>%
        dplyr::select(item_id, WOE.item_id) %>% distinct
    WOE.item_size <- traindf %>%
        dplyr::select(item_size, WOE.item_size) %>% distinct
    WOE.brand_id <- traindf %>%
        dplyr::select(brand_id, WOE.brand_id) %>% distinct
    WOE.basket.size <- traindf %>%
        dplyr::select(basket.size, WOE.basket.size) %>% distinct
    
    # apply WOE labels to test set
    testdf <- testdf %>%
        left_join(WOE.user_id, "user_id") %>%
        left_join(WOE.item_id, "item_id") %>%
        left_join(WOE.item_size, "item_size") %>%
        left_join(WOE.brand_id, "brand_id") %>% 
        left_join(WOE.basket.size, "basket.size")
    
    # 0 out NA's
    testdf[is.na(testdf)] <- 0
    
    # select right variables TO PREDICT
    traindf <- traindf %>%
        dplyr::select(
            # DEMOGRAPHIC VARS
            age, 
            account.age.order,
            WOE.user_id, # WOE
            user.total.items, user.total.expen,
            # BASKET VARS
            deliver.time, 
            basket.big, WOE.basket.size, 
            item.basket.size.same, item.basket.size.diff, 
            item.basket.same.category,
            no.return,
            # ITEM VARS
            WOE.item_id, WOE.item_size, WOE.brand_id, # WOE
            discount.pc, 
            item_price, 
            return)
    
    if (final == TRUE) { # no return variable for final
        testdf <- testdf %>%
            dplyr::select(
                # DEMOGRAPHIC VARS
                age, 
                account.age.order,
                WOE.user_id, # WOE
                user.total.items, user.total.expen,
                # BASKET VARS
                deliver.time, 
                basket.big, WOE.basket.size, 
                item.basket.size.same, item.basket.size.diff, 
                item.basket.same.category,
                no.return,
                # ITEM VARS
                WOE.item_id, WOE.item_size, WOE.brand_id, # WOE
                discount.pc, 
                item_price)
    } else {
        testdf <- testdf %>%
            dplyr::select(
                # DEMOGRAPHIC VARS
                age, 
                account.age.order,
                WOE.user_id, # WOE
                user.total.items, user.total.expen,
                # BASKET VARS
                deliver.time, 
                basket.big, WOE.basket.size, 
                item.basket.size.same, item.basket.size.diff, 
                item.basket.same.category,
                no.return,
                # ITEM VARS
                WOE.item_id, WOE.item_size, WOE.brand_id, # WOE
                discount.pc, 
                item_price, 
                return)
    }
    
    
    return(list(traindf = traindf, testdf = testdf))
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