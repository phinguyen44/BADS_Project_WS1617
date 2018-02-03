################################################################################
# TrainFinalModel.R
#
################################################################################
# Description:
# 
# BADS project - train final model
#
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"documents/projects/bads-ws1718-group21")
setwd(wd)

# List all packages needed for session
neededPackages <- c("tidyverse", "magrittr", "purrr", "infuser",
                    "caret", "mlr", 
                    "xgboost", "gbm", "rpart", "e1071", "MASS", "nnet",
                    "pROC", "parallel", "parallelMap")
allPackages    <- c(neededPackages %in% installed.packages()[,"Package"])

# Install packages (if not already installed)
if (!all(allPackages)) {
    missingIDX <- which(allPackages == FALSE)
    needed     <- neededPackages[missingIDX]
    lapply(needed, install.packages)  
}

# Load all defined packages
lapply(neededPackages, function(x) suppressPackageStartupMessages(
    library(x, character.only = TRUE)))

# Load data
load("Data/BADS_WS1718_known_ready.RData")
load("Data/BADS_WS1718_class_ready.RData")
load("Data/CalibratedThreshold.Rdata")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# INITIAL SETUP

# TODO: maybe discount.pc should not be factor, same with item.basket.size...

# reorder and select variables
df.train <- dat.ready %>%
    dplyr::mutate(return    = as.integer(levels(return))[return],
                  no.return = as.factor(no.return)) %>% 
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        user_id, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, basket.size, 
        item.basket.size.same, item.basket.size.diff, item.basket.same.category,
        no.return,
        # ITEM VARS
        item_id, item_size, brand_id, # WOE
        discount.pc, 
        item_price, 
        return)

df.class <- dat.test %>%
    # dplyr::mutate(return = as.integer(levels(return))[return]) %>% 
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        user_id, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, basket.size, 
        item.basket.size.same, item.basket.size.diff, item.basket.same.category,
        no.return,
        # ITEM VARS
        item_id, item_size, brand_id, # WOE
        discount.pc, 
        item_price, 
        return)

# SAVE DATA FOR LATER
df.label <- df.train$return
df.price <- df.train$item_price

class.price <- df.class$item_price

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

# standardize
df.train <- z.scale(df.train)
df.class <- z.scale(df.class)

# add in WOE variables
df.train$user_id_WOE <- WOE(df.train, "user_id")
df.train$item_id_WOE <- WOE(df.train, "item_id")
df.train$item_size_WOE <- WOE(df.train, "item_size")
df.train$brand_id_WOE <- WOE(df.train, "brand_id")

user_id_WOE <- df.train %>%
    dplyr::select(user_id, user_id_WOE) %>% distinct
item_id_WOE <- df.train %>%
    dplyr::select(item_id, item_id_WOE) %>% distinct
item_size_WOE <- df.train %>%
    dplyr::select(item_size, item_size_WOE) %>% distinct
brand_id_WOE <- df.train %>%
    dplyr::select(brand_id, brand_id_WOE) %>% distinct

# apply WOE labels to test set
df.class <- df.class %>%
    left_join(user_id_WOE, "user_id") %>%
    left_join(item_id_WOE, "item_id") %>%
    left_join(item_size_WOE, "item_size") %>%
    left_join(brand_id_WOE, "brand_id")

# 0 out NA's
df.class[is.na(df.class)] <- 0

# select right variables for dataset
# select right variables for dataset
df.train <- df.train %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        user_id_WOE, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, basket.size, 
        item.basket.size.same, item.basket.size.diff, 
        item.basket.same.category,
        no.return,
        # ITEM VARS
        item_id_WOE, item_size_WOE, brand_id_WOE, # WOE
        discount.pc, 
        item_price, 
        return)

df.class <- df.class %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        user_id_WOE, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, basket.size, 
        item.basket.size.same, item.basket.size.diff, 
        item.basket.same.category,
        no.return,
        # ITEM VARS
        item_id_WOE, item_size_WOE, brand_id_WOE, # WOE
        discount.pc, 
        item_price, 
        return)

# TRAIN MODEL
fin   <- map2(mods, learners, function(f, x) f(x, df.train, df.class, calib = TRUE))

# APPLY NEW THRESHOLD
fin.new.calib <- map2(fin, thresh.mean.l.calib,
                      function(x,y) setThreshold(x$pred.calib,y))

# GET PREDICTIONS
# pred   <- lapply(fin, function(x) x$pred$data$prob.1)
# pred.c <- lapply(fin, function(x) x$pred.calib$data$prob.1)

# GET HYPERPARAMETERS
hp.all    <- lapply(fin[2:4], function(x) x$pars)

################################################################################
# ENSEMBLE



################################################################################
# PREDICTION

# TODO: how do we report? do we estimate what 'final cost' per cust would be?
# or maybe final cost per mistake?