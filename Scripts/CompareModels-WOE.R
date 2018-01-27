################################################################################
# CompareModels.R
#
################################################################################
# Description:
# BADS project - compare models with cross-validation to estimate out-of-sample
# error. includes WOE
#
# now does feature selection for each model (wrapper? filter?)
# and platt scaling as option
#
# TODO: remove duplicate variables (e.g. dummy and numeric equivalents)
#
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("USERPROFILE"),"/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, magrittr, purrr, infuser,
      caret, mlr, xgboost, gbm, rpart, e1071, MASS, nnet,
      mice, pROC, parallel, parallelMap)

# Load data
load("Data/BADS_WS1718_known_var.RData")
df.known <- read.csv("Data/BADS_WS1718_known.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# FEATURES

# select variables
df.train <- dat.input1 %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, age.group,
        user_state, user_title, WestGerm, income.ind,
        first.order, account.age.order,
        user_id, # WOE
        # BASKET VARS
        deliver.time, order_year, order_month, weekday, no.return,
        basket.big, basket.size, basket.value,
        order.same.item, item.basket.size.diff, item.basket.same.category,
        item.basket.category.size.diff,
        order.same.itemD, item.basket.size.diffD, item.basket.same.categoryD,
        item.basket.category.size.diffD,
        # ITEM VARS
        item_id, item_color, item_size, brand_id, # WOE
        brand.cluster, item.color.group, item.category, item.subcategory,
        discount.abs, discount.pc, is.discount,
        item_price, item_priceB, price.inc.ratio,
        return)

# TODO: DELETE
# TEST
df.train <- df.train[1:10000, ]

################################################################################
# INITIAL SPLIT

# SPLIT DATA
set.seed(321)
idx.train <- createDataPartition(y = df.train$return, p = 0.8, list = FALSE)
tr <- df.train[idx.train, ]   # training set
ts <- df.train[-idx.train, ]  # test set

tr.label <- tr$return
ts.label <- ts$return

## LIST OF FUNCTIONS
learners <- list(lr = "classif.logreg",
                 nn  = "classif.nnet")#,
                 #xgb = "classif.xgboost",
                 #rf = "classif.randomForest")
mods <- list(lr = lr.mod,
             nn  = nn.mod)#,
             #xgb = xgb.mod,
             #rf = rf.mod)

ts.price <- ts$item_price[-idx.train]

################################################################################
# CROSS-VALIDATION

# split training set into k-folds
# inner loop calculates WOE for each split, does hyperparameter tuning
# outer loop estimates out-of-sample performance
# final model is made by fitting model (including hyperparameter tuning) to whole data set

start1 <- Sys.time()

k <- 5
folds <- createFolds(tr$return, k = k, list = TRUE)
str(folds)

foldruntime <- rep(0, k)
yhat        <- vector("list", length = k)
yhat.r      <- yhat
actual      <- yhat
ts.price.f  <- yhat

for (i in 1:k) {

    start <- Sys.time()

    tr.f <- tr[-folds[[i]], ] # in CV training
    ts.f <- tr[folds[[i]], ]  # in CV test

    tr.label.f <- tr.f$return
    ts.label.f <- ts.f$return
    
    ts.price.f[[i]] <- ts.f$item_price

    # add in WOE variables
    tr.f$user_id_WOE <- WOE(tr.f, "user_id")
    tr.f$item_id_WOE <- WOE(tr.f, "item_id")
    tr.f$item_color_WOE <- WOE(tr.f, "item_color")
    tr.f$item_size_WOE <- WOE(tr.f, "item_size")
    tr.f$brand_id_WOE <- WOE(tr.f, "brand_id")

    user_id_WOE <- tr.f %>% dplyr::select(user_id, user_id_WOE) %>% distinct
    item_id_WOE <- tr.f %>% dplyr::select(item_id, item_id_WOE) %>% distinct
    item_color_WOE <- tr.f %>% dplyr::select(item_color, item_color_WOE) %>%
        distinct
    item_size_WOE <- tr.f %>% dplyr::select(item_size, item_size_WOE) %>%
        distinct
    brand_id_WOE <- tr.f %>% dplyr::select(brand_id, brand_id_WOE) %>% distinct

    # apply WOE labels to test set
    ts.f <- ts.f %>%
        left_join(user_id_WOE, "user_id") %>%
        left_join(item_id_WOE, "item_id") %>%
        left_join(item_color_WOE, "item_color") %>%
        left_join(item_size_WOE, "item_size") %>%
        left_join(brand_id_WOE, "brand_id")

    # 0 out NA's
    ts.f[is.na(ts.f)] <- 0

    # select right variables for dataset
    tr.f <- tr.f %>%
        dplyr::select(
            # DEMOGRAPHIC VARS
            age, age.group,
            user_state, user_title, WestGerm, income.ind,
            first.order, account.age.order,
            user_id_WOE,
            # BASKET VARS
            deliver.time, order_year, order_month, weekday, no.return,
            basket.big, basket.size, basket.value,
            order.same.item, item.basket.size.diff, item.basket.same.category,
            item.basket.category.size.diff,
            order.same.itemD,item.basket.size.diffD, item.basket.same.categoryD,
            item.basket.category.size.diffD,
            # ITEM VARS
            item_id_WOE, item_color_WOE, item_size_WOE, brand_id_WOE,
            brand.cluster, item.color.group, item.category, item.subcategory,
            discount.abs, discount.pc, is.discount,
            item_price, item_priceB, price.inc.ratio,
            return)

    ts.f <- ts.f %>%
        dplyr::select(
            # DEMOGRAPHIC VARS
            age, age.group,
            user_state, user_title, WestGerm, income.ind,
            first.order, account.age.order,
            user_id_WOE,
            # BASKET VARS
            deliver.time, order_year, order_month, weekday, no.return,
            basket.big, basket.size, basket.value,
            order.same.item, item.basket.size.diff, item.basket.same.category,
            item.basket.category.size.diff,
            order.same.itemD,item.basket.size.diffD, item.basket.same.categoryD,
            item.basket.category.size.diffD,
            # ITEM VARS
            item_id_WOE, item_color_WOE, item_size_WOE, brand_id_WOE,
            brand.cluster, item.color.group, item.category, item.subcategory,
            discount.abs, discount.pc, is.discount,
            item_price, item_priceB, price.inc.ratio,
            return)

    # make model task
    traint.f <- makeClassifTask(data = tr.f, target = "return", positive = 1)
    testt.f  <- makeClassifTask(data = ts.f, target = "return", positive = 1)

    # TRAIN MODEL
    yhat[[i]]   <- map2(mods, learners,
                        function(f, x) f(x, tr.f, ts.f, calib=TRUE))

    # GET ACTUAL VALUES AND STORE THEM
    actual[[i]] <- ts.label.f

    end <- Sys.time()
    foldruntime[i] <- end - start

}

# Check stability of cross-validation (metaparameters, error)
alldata  <- transpose(yhat)
alldata2 <- lapply(alldata, transpose)

# predictions for each model
preds   <- lapply(alldata2, 
                  function(x) lapply(x$pred, function(y) y$data$prob.1))
preds.r <- lapply(preds, 
                  function(x) lapply(x, function(y) round(y)))
p.calib <- lapply(alldata2, 
                  function(x) lapply(x$pred.calib, function(y) y$data$prob.1))
p.calib.r <- lapply(p.calib, 
                    function(x) lapply(x, function(y) round(y)))

# get prediction accuracy
get.acc  <- function(x, act) confusionMatrix(x, act, positive="1")$overall[1]
pred.acc <- lapply(preds.r, function(x) map2_dbl(x, actual, get.acc))
acc.mean <- lapply(pred.acc, mean)
acc.se   <- lapply(pred.acc, sd)

acc.mean
acc.se

# Adjust Threshold

thresh.list = preds

for(i in 1:length(learners)){ # 2
  
  d = data.frame()
  
  for(j in 1:k){  # 5
    print(i)
    print(j)
    
    initial = find.threshold(act = actual[[j]], pred = preds[[i]][[j]], cost = ts.price.f[[j]])
    d = rbind(d, initial)
    
    print(d)
  }
  
  thresh.list[[i]] <- d
  
}

thresh.mean.l = lapply(thresh.list, function(z){
  
  m = mean(z$threshold)
  
  return(m)
  
})

setThreshold(yhat[[1]]$lr$pred, thresh.mean.l$lr[1])



# get prediction accuracy for calibrated results
pred.acc.c <- lapply(p.calib.r, function(x) map2_dbl(x, actual, get.acc))
acc.mean.c <- lapply(pred.acc.c, mean)
acc.se.c   <- lapply(pred.acc.c, sd)

acc.mean.c
acc.se.c

# hyperparameters (examine)
hp    <- lapply(alldata2[2:4], function(x) lapply(x$pars, function(y) y))
hp.t  <- lapply(hp, transpose)
hp.df <- lapply(hp.t, function(x)
    data.frame(matrix(unlist(x), ncol = length(x))))
colnames(hp.df$nn)  <- names(hp.t$nn)
colnames(hp.df$xgb) <- names(hp.t$xgb)
colnames(hp.df$rf)  <- names(hp.t$rf)

hp.df$nn
hp.df$xgb
hp.df$rf

# TODO: APPLY COST MATRIX

################################################################################
# TRAIN FINAL MODEL

# add in WOE variables
tr$user_id_WOE    <- WOE(tr, "user_id")
tr$item_id_WOE    <- WOE(tr, "item_id")
tr$item_color_WOE <- WOE(tr, "item_color")
tr$item_size_WOE  <- WOE(tr, "item_size")
tr$brand_id_WOE   <- WOE(tr, "brand_id")

user_id_WOE    <- tr %>% dplyr::select(user_id, user_id_WOE) %>% distinct
item_id_WOE    <- tr %>% dplyr::select(item_id, item_id_WOE) %>% distinct
item_color_WOE <- tr %>% dplyr::select(item_color, item_color_WOE) %>%
    distinct
item_size_WOE  <- tr %>% dplyr::select(item_size, item_size_WOE) %>%
    distinct
brand_id_WOE   <- tr %>% dplyr::select(brand_id, brand_id_WOE) %>% distinct

# apply WOE labels to test set
ts <- ts %>%
    left_join(user_id_WOE, "user_id") %>%
    left_join(item_id_WOE, "item_id") %>%
    left_join(item_color_WOE, "item_color") %>%
    left_join(item_size_WOE, "item_size") %>%
    left_join(brand_id_WOE, "brand_id")

# 0 out NA's
ts[is.na(ts)] <- 0

# select right variables for dataset
tr <- tr %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age.group,
        user_state, user_title, WestGerm, income.ind,
        first.order, account.age.order,
        user_id_WOE,
        # BASKET VARS
        deliver.time, order_year, order_month, weekday, no.return,
        basket.size, basket.value,
        order.same.itemD,item.basket.size.diffD, item.basket.same.categoryD,
        item.basket.category.size.diffD,
        # ITEM VARS
        item_id_WOE, item_color_WOE, item_size_WOE, brand_id_WOE,
        brand.cluster, item.category, item.subcategory,is.discount,
        item_priceB, price.inc.ratio,
        return)

ts <- ts %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age.group,
        user_state, user_title, WestGerm, income.ind,
        first.order, account.age.order,
        user_id_WOE,
        # BASKET VARS
        deliver.time, order_year, order_month, weekday, no.return,
        basket.size, basket.value,
        order.same.itemD,item.basket.size.diffD, item.basket.same.categoryD,
        item.basket.category.size.diffD,
        # ITEM VARS
        item_id_WOE, item_color_WOE, item_size_WOE, brand_id_WOE,
        brand.cluster, item.category, item.subcategory,is.discount,
        item_priceB, price.inc.ratio,
        return)

# TRAIN MODEL
fin   <- map2(mods, learners, function(f, x) f(x, tr, ts, calib = TRUE))

# GET PREDICTIONS
pred   <- lapply(fin, function(x) x$pred$data$prob.1)
pred.r <- lapply(pred, round)
cMat   <- lapply(pred.r,
                 function(x) confusionMatrix(x, ts.label, positive = "1"))
cMat

# GET CALIBRATED PREDICTIONS
pred.c   <- lapply(fin, function(x) x$pred.calib$data$prob.1)
pred.r.c <- lapply(pred.c, round)
cMat.c   <- lapply(pred.r.c,
                   function(x) confusionMatrix(x, ts.label, positive = "1"))
cMat.c

# TODO: reliability plots

# SAVE prediction results (on test set)
fin.name  <- infuse("Data/Predictions - Phi/run_{{rundate}}_yhat.Rdata",
                    rundate = strftime(Sys.Date(), "%Y%m%d"))
act.name  <- infuse("Data/Predictions - Phi/run_{{rundate}}_y.Rdata",
                    rundate = strftime(Sys.Date(), "%Y%m%d"))

save(pred, file = fin.name)
save(ts.label, file = act.name)

end1 <- Sys.time()
end1-start1

################################################################################
# ENSEMBLE

# TODO: after optimizing cost matrix

################################################################################
# BENCHMARK PLOTS

# Reliability plots to compare original vs calibrated results
for (i in 1:length(pred)) reliability.plot(ts.label, pred[[i]], pred.c[[i]], 10)

# TODO: solve resizing issues, add titles
# does calibration improve estimates?

################################################################################
# PREDICTION
