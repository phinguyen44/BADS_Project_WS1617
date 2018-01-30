################################################################################
# TrainFinalModel.R
#
################################################################################
# Description:
# 
# BADS project - train final model
# 
# TODO: need to save elements from cross-validation that are needed to train
#
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"documents/projects/bads-ws1718-group21")
setwd(wd)

# TODO: CHANGE THIS TO BE MORE USABLE FOR ALL PARTIES
# Load packages
needs(tidyverse, magrittr, purrr, infuser,
      caret, mlr, xgboost, gbm, rpart, e1071, MASS, nnet,
      mice, pROC, parallel, parallelMap)

# Load data
load("Data/BADS_WS1718_known_ready.RData")
df.known   <- read.csv("Data/BADS_WS1718_known.csv")
df.unknown <- read.csv("Data/BADS_WS1718_class_20180115.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# INITIAL SETUP

# reorder and select variables
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

# SAVE DATA FOR LATER
df.label <- df.train$return
df.price <- df.train$item_price

## LIST OF FUNCTIONS
learners <- list(lr = "classif.logreg",
                 rf = "classif.randomForest",
                 nn  = "classif.nnet",
                 xgb = "classif.xgboost")
mods <- list(lr = lr.mod,
             rf = rf.mod,
             nn  = nn.mod,
             xgb = xgb.mod)

################################################################################
# TRAIN FINAL MODEL

# add in WOE variables
df.train$user_id_WOE    <- WOE(df.train, "user_id")
df.train$item_id_WOE    <- WOE(df.train, "item_id")
df.train$item_color_WOE <- WOE(df.train, "item_color")
df.train$item_size_WOE  <- WOE(df.train, "item_size")
df.train$brand_id_WOE   <- WOE(df.train, "brand_id")

user_id_WOE    <- df.train %>% 
    dplyr::select(user_id, user_id_WOE) %>% distinct
item_id_WOE    <- df.train %>% 
    dplyr::select(item_id, item_id_WOE) %>% distinct
item_color_WOE <- df.train %>% 
    dplyr::select(item_color, item_color_WOE) %>%distinct
item_size_WOE  <- df.train %>% 
    dplyr::select(item_size, item_size_WOE) %>%distinct
brand_id_WOE   <- df.train %>% 
    dplyr::select(brand_id, brand_id_WOE) %>% distinct

# TODO: THIS ... NEEDS TO BE UPDATED FOR UNKNOWN SET
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
        age,
        user_state, user_title, 
        first.order, account.age.order,
        user_id_WOE,
        # BASKET VARS
        deliver.time, order_month, weekday, no.return,
        basket.size,
        order.same.itemD,item.basket.size.diffD, 
        # ITEM VARS
        item_id_WOE, brand_id_WOE,
        item.category, item.subcategory,
        is.discount,
        item_priceB, price.inc.ratio,
        return)

# TODO: for final data set
ts <- ts %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age,
        user_state, user_title, 
        first.order, account.age.order,
        user_id_WOE,
        # BASKET VARS
        deliver.time, order_month, weekday, no.return,
        basket.size,
        order.same.itemD,item.basket.size.diffD, 
        # ITEM VARS
        item_id_WOE, brand_id_WOE,
        item.category, item.subcategory,
        is.discount,
        item_priceB, price.inc.ratio,
        return)

# TRAIN MODEL
fin   <- map2(mods, learners, function(f, x) f(x, tr, ts, calib = TRUE))

# # APPLY NEW THRESHOLD
# fin.new       <- map2(fin, thresh.mean.l, function(x,y) setThreshold(x$pred,y))
# fin.new.calib <- map2(fin, thresh.mean.l.calib, 
#                       function(x,y) setThreshold(x$pred.calib,y))
# 
# # get accuracy and cost
# final.acc  <- map2(fin.new, thresh.mean.l, function(x,y) acc.calc(y, ts.label, pred = x$data$prob.1))
# final.cost <- map2(fin.new, thresh.mean.l, function(x,y) cost.calc(y, ts.label, pred = x$data$prob.1, cost = ts.price))
# 
# final.acc.calib  <- map2(fin.new.calib, thresh.mean.l.calib, 
#                          function(x,y) acc.calc(y, ts.label, x$data$prob.1))
# final.cost.calib <- map2(fin.new.calib, thresh.mean.l.calib, 
#                          function(x,y) cost.calc(y, 
#                                                  ts.label, 
#                                                  x$data$prob.1, 
#                                                  ts.price))

# GET PREDICTIONS
pred   <- lapply(fin, function(x) x$pred$data$prob.1)
pred.c <- lapply(fin, function(x) x$pred.calib$data$prob.1)

# GET HYPERPARAMETERS
hp.all    <- lapply(fin[2:4], function(x) x$pars)

################################################################################
# PREDICTION

# TODO: how do we report? do we estimate what 'final cost' per cust would be?
# or maybe final cost per mistake?