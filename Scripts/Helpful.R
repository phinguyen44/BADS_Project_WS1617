################################################################################
# Helpful.R
#
# Phi Nguyen: phi.nguyen@outlook.com
#
################################################################################
# Description:
# Helpful functions designed to aid in BADS project
#
# DATA EXPLORATION:
# dist.check() - compares distribution of cat variables in training & test sets
# return.check() - looks at return rates by category in training set
# num.check() - looks at return rates for a numerical variable in training set 
# discrete.bins() - organizes observations into selected # of bins
# discrete.power() - organizes observations into bins (bins organized in size 
# by power)
# assign.bins() - creates bins based off bins created in discrete.bins()
# samplefxn() - imputation fxn (misnomer)
# 
# MODEL BUILDING:
# build.glm() - builds predictions and classification table for glm model
# reliability.plot() - diagram to assess how 'calibrated' a classifier is
# platt() - platt scaling (performing logistic regression on the classifier output to calibrate model output)
# 
# MODEL EVALUATION:
# performance.met() - calculates basic classification table stuff
# log.loss() - calculates log loss error
# brier.score() - calculates brier score
# cost.fxn() - calculates cost function as described in paper
# rev.gain.fxn() - calculates revenue gain over case where no message is sent
# 
# TODO: ROCINFO: http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html#interpretation-and-reporting - for imbalanced cost functions
# 
################################################################################

################################################################################
# DATA EXPLORATION

# check distribution of categorical variables (and how they might differ)
dist.check <- function(df.train, var) {
    # TRANSFORM INCORRECTLY SCALED VARIABLES
    if (var == "brand_id") df.test[[var]] <- df.test[[var]] - 100
    if (var == "item_id") df.test[[var]] <- (df.test[[var]] / 2) - 1
    dist.train <- data.frame(
        train = table(df.train[[var]])[order(table(df.train[[var]]))]
        )
    dist.test <- data.frame(
        test = table(df.test[[var]])[order(table(df.test[[var]]))]
        )
  
    dister   <- dist.train %>% 
        full_join(dist.test, by = c("train.Var1" = "test.Var1"))
  
    dister[is.na(dister)]    <- 0
    dister$Difference        <- 2 * dister[, 3] / dister[, 2] # should be near 1
  
    names(dister$train.Var1) <- "Variable"
    return(dister)
}

# check return rates of cat variables
return.check <- function(df, var) {
    return.table <- as.data.frame(as.matrix.data.frame(
        table(df[[var]], df$return)
        ))
    return.table[[var]] <- levels(df[[var]])
    return.table <- return.table %>% 
        select(var, V1, V2) %>% 
        rename(Keep = V1, Return = V2) %>% 
        mutate(Total = Keep + Return, ReturnRate = round(Return / Total, 3))
  
    return(return.table)
}

# check return rates of numeric variables
num.check <- function(df, var) {
    prices.df <- df %>% arrange(get(var))
    tables    <- as.data.frame(table(prices.df[[var]], prices.df$return))
    tables    <- tidyr::spread(tables, Var2, Freq)
    tables    <- tables %>% 
        rename(Keep = '0', Return = '1') %>% 
        mutate(Total      = Keep + Return, 
               ReturnRate = round(Return / Total, 3),
               Var1       = as.numeric(levels(Var1))[Var1])
  
  return(tables)
}

# Function creates discrete buckets based on chosen variable
# Note that fxn will try to create numbins selected, but if some observations 
# span multiple bins, there may be fewer bins
discrete.bin <- function(df, variable, numbins = 10) {
    df <- df %>% 
        arrange(df[[variable]]) %>% 
        mutate(allsums = cumsum(Total))
    
    cutoff  <- round(tail(df, 1)$allsums / numbins)
    binsmax <- as.integer(seq(cutoff, tail(df, 1)$allsums, by = cutoff))
    if (length(binsmax) < numbins) {binsmax <- c(binsmax, tail(df, 1)$allsums)}
    
    # last value underbins
    binidx  <- sapply(binsmax, function(x) last(which(df$allsums <= x))) 
    
    maxval <- df[[variable]][binidx]
    maxval <- c(0, maxval)
    
    df$bins  <- paste0("[0, ", maxval[2], "]")
    
    for (i in 2:length(maxval)) {
        for (j in 1:nrow(df)) {
            if (df[[variable]][j] > maxval[i]) {
                df$bins[j] <- paste0("(", maxval[i], ", ", maxval[i+1], "]")
            }
        } 
    }
    
    df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
        group_by(bins) %>% 
        dplyr::summarize(ReturnRate = sum(Return) / sum(Total))
    
    return(df.bins)
}

# create a discrete function that follows power
discrete.power <- function(df, variable, numbins = 10, powerval = 5) {
    df <- df %>% 
    arrange(df[[variable]])
    
    cutoffs <- powerval ^ (1:numbins)
    if (max(cutoffs) > max(df[[variable]])) {
        message("Bin values exceeds num items in group. Truncating # of bins.")
        cutoffs <- cutoffs[cutoffs < max(df[[variable]])] 
    }
    groupings    <- c(0, cutoffs, max(df[[variable]]))
    
    df$bins  <- paste0("[0, ", groupings[2], "]")
    
    for (i in 2:length(groupings)) {
        for (j in 1:nrow(df)) {
            if (df[[variable]][j] > groupings[i]) {
                df$bins[j] <- paste0("(", groupings[i], ", ", 
                                     groupings[i+1], "]")
            }
        }
    }
    
    df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
        group_by(bins) %>% 
        dplyr::summarize(ReturnRate = sum(Return) / sum(Total))
    
    return(df.bins)
}

# Create buckets in dataset
assign.bins <- function(df, buckets, variable) {
    start    <- unlist(gregexpr(buckets$bins, pattern = ", "))
    end      <- unlist(gregexpr(buckets$bins, pattern = "]"))
    ceilings <- c(0, as.numeric(substr(buckets$bins, start + 2, end - 1)))
    
    # set arbitrarily large ceiling
    ceilings[length(ceilings)] <- 99999
    
    grouping <- cut(df[[variable]], ceilings, include.lowest = TRUE)
    return(grouping)
}

# imputation
samplefxn <- function(df, var, type) {
    idx <- is.na(df[[var]])
    len <- sum(idx)
    
    values <- switch(type,
                   sample = sample(df[!idx, var], size = len, replace = TRUE),
                   mean   = rep(round(mean(df[[var]], na.rm = TRUE)), 
                                times = len))
    
    return(values)
}

################################################################################
# MODEL BUILDING

# build glm model (with new platt scale feature!)
build.glm <- function(mod, trainset, testset, alpha, platt.scaling = FALSE) {
    
    if (platt.scaling) {
        trainset.n  <- trainset
        samplecalib <- sample(1:nrow(trainset.n), 10000, replace = FALSE)
        trainset    <- trainset.n[-samplecalib, ]
        calibset    <- trainset.n[samplecalib, ]
    }
    
    matrix.x  <- model.matrix(mod, data = trainset)
    mod1      <- cv.glmnet(x = matrix.x, y = trainset$return, alpha = alpha, 
                         family = "binomial", standardize = TRUE)
    # plot(mod1)
    # mod1$lambda.1se
    coefff = coef(mod1, s = "lambda.1se")
    
    # Make prediction
    new.x <- model.matrix(mod, data = testset)
    pred  <- predict(mod1, newx = new.x, s = "lambda.1se", type = "response")
    
    test  <- data.frame(pred, testset$return, round(pred))
    names(test) <- c("prob", "actual", "result")
    
    # Perform platt scaling
    if (platt.scaling) {
        # Make prediction
        new.xc  <- model.matrix(mod, data = calibset)
        pred.c  <- predict(mod1, newx = new.xc, s="lambda.1se", type="response")
        test.c  <- data.frame(pred.c, calibset$return, round(pred.c))
        names(test.c) <- c("prob", "actual", "result")
        
        # Predictions after platt scaling
        new.pred    <- platt(test.c$actual, test.c$prob)
        test.c$prob <- new.pred
    
    }
    
    final <- list(mod = mod1, Coef = coefff, Results = test)
    if (platt.scaling) final[["Results.Platt"]] <- test.c
    
    return(final)
}

reliability.plot <- function(act, pred, bins = 10) {
    # act: vector of actual values. 0 or 1
    # pred: vector of predictions. real number between 0 and 1
    # bins: number of bins to use
    if(!require("Hmisc")) install.packages("Hmisc")
    library(Hmisc)
    
    bin.pred <- cut(pred, bins)
    df       <- data.frame(act = act, pred = pred, bin = bin.pred)
    grouped  <- df %>% 
        dplyr::group_by(bin) %>% 
        dplyr::summarize(x = sum(act) / n(), 
                         y = mean(pred))
    
    plot(grouped$y, grouped$x, 
         xlim = c(0,1), 
         ylim = c(0,1), 
         xlab = "Mean Prediction", 
         ylab = "Observed Fraction", 
         col  = "red", type = "o", main = "Reliability Plot")
    lines(c(0,1), c(0,1), col = "grey")
    subplot(hist(pred, xlab = "", ylab = "", main = "", xlim = c(0,1), 
                 col="blue"), 
            grconvertX(c(.8, 1), "npc"), grconvertY(c(0.08, .25), "npc"))
    
}

platt <- function(act, pred) {
    
    ##### THESE STEPS IN MODEL BUILD
    # splits training data again into model training and calibration (with platt == TRUE arg in function)
    # train model on test set
    # predict and score test set (logloss)
    #####
    
    # train the calibration model on calibration set using logistic regression
    # predict and score calibration set after platt scaling
    
    calib   <- data.frame(y = act, x = pred)
    model   <- glm(y ~ x, calib, family = binomial)
    
    # predicting on the cross validation after platt scaling
    results <- predict(model, calib, type = "response")
    return(results)
}

################################################################################
# MODEL EVALUATION

performance.met <- function(act, pred) {
    # Check performance
    logloss <- log.loss(act, pred)
    
    # Convert predicted values to 1/0 for classification table
    pred    <- round(pred)
    check1  <- table(predicted = pred, actual = act)
    mce1    <- 1 - sum(diag(check1)) / sum(check1)
    
    # GET FPR / FNR
    test <- data.frame(act = act, pred = pred)
    test$Class <- with(data = test, ifelse(
        act == pred & act == 1, "TP", ifelse(
            act == pred & act == 0, "TN", ifelse(
                act != pred & act == 1, "FN", "FP")
            )
        ) 
    )
    
    FPR <- test %>% 
        filter(act == 0) %>% 
        dplyr::summarize(FPR = sum(pred) / n()) %>% 
        as.numeric()
    
    FNR <- test %>% 
        filter(act == 1) %>% 
        dplyr::summarize(FNR = (n() - sum(pred)) / n()) %>% 
        as.numeric()
    
    final <- list(ClassTable = check1, FPR = FPR, FNR = FNR, 
                  MCE = mce1, LogLoss = logloss)
    return(final)
}

log.loss <- function(act, pred) {
    eps  <- 1e-15
    nr   <- length(pred)
    pred <- matrix(sapply(pred, function(x) max(eps,x)), nrow = nr) 
    pred <- matrix(sapply(pred, function(x) min(1-eps,x)), nrow = nr)
    ll   <- sum(act*log(pred) + (1-act)*log(1-pred))
    ll   <-  ll * -1/(length(act)) 
    return(ll)
}

brier.score <- function(act, pred) {
    nr    <- length(pred)
    brier <- (1/nr) * sum((pred - act)^2)
    return(brier)
}

rev.gain.fxn <- function(act, pred, cost) {
    fp <- (1-act) * pred * (-0.5) * cost   # revenue lost from FP misclass
    tp <- act * pred * 0.5 * (3+0.1*cost)  # revenue gain from TP class
    return(sum(fp + tp))
}

cost.fxn <- function(act, pred, cost) {
    fp <- (1-act) * pred * (-0.5) * cost           # rev lost from FP misclass
    fn <- act * (1 - pred) * (-0.5) * (3+0.1*cost) # rev lost from FN misclass
    return(sum(fp + fn))
}
