################################################################################
# TrainFinalModel.R
#
################################################################################
# Description:
# 
# BADS project - train final model

# TODO: UPDATE THIS. scale / transform manually (since return isn't there)
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
load("Data/BADS_WS1718_class_ready.RData")
dat.test <- dat.ready
load("Data/BADS_WS1718_known_ready.RData")
load("Data/CalibratedThreshold.Rdata")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# INITIAL SETUP

# reorder and select variables
df.train <- dat.ready %>%
    dplyr::mutate(no.return = as.factor(no.return)) %>% 
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
        item_price)

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
df.train$WOE.user_id     <- WOE(df.train, "user_id")
df.train$WOE.item_id     <- WOE(df.train, "item_id")
df.train$WOE.item_size   <- WOE(df.train, "item_size")
df.train$WOE.brand_id    <- WOE(df.train, "brand_id")
df.train$WOE.basket.size <- WOE(df.train, "basket.size")

WOE.user_id <- df.train %>%
    dplyr::select(user_id, WOE.user_id) %>% distinct
WOE.item_id <- df.train %>%
    dplyr::select(item_id, WOE.item_id) %>% distinct
WOE.item_size <- df.train %>%
    dplyr::select(item_size, WOE.item_size) %>% distinct
WOE.brand_id <- df.train %>%
    dplyr::select(brand_id, WOE.brand_id) %>% distinct
WOE.basket.size <- df.train %>%
    dplyr::select(basket.size, WOE.basket.size) %>% distinct

# apply WOE labels to test set
df.class <- df.class %>%
    left_join(WOE.user_id, "user_id") %>%
    left_join(WOE.item_id, "item_id") %>%
    left_join(WOE.item_size, "item_size") %>%
    left_join(WOE.brand_id, "brand_id") %>% 
    left_join(WOE.basket.size, "basket.size")

# 0 out NA's
df.class[is.na(df.class)] <- 0

# select right variables for dataset
df.train <- df.train %>%
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

df.class <- df.class %>%
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        WOE.user_id, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, WOE.basket.size, # WOE 
        item.basket.size.same, item.basket.size.diff, 
        item.basket.same.category,
        no.return,
        # ITEM VARS
        WOE.item_id, WOE.item_size, WOE.brand_id, # WOE
        discount.pc, 
        item_price)

# TRAIN MODEL
fin   <- map2(mods, learners, function(f, x) f(x, df.train, df.class, calib = TRUE))

# APPLY NEW THRESHOLD
fin.new.calib <- map2(fin, thresh.mean.l.calib,
                      function(x,y) setThreshold(x$pred.calib,y))

# GET PREDICTIONS
pred <- lapply(fin.new.calib, 
               function(x) as.numeric(as.character(x$data$response)))

# GET HYPERPARAMETERS
hp.all    <- lapply(fin[2:4], function(x) x$pars)
unlist(hp.all$rf)
unlist(hp.all$nn)
unlist(hp.all$xgb)

################################################################################
# ENSEMBLE

# BEST MODEL IS ONE FROM CROSS-VALIDATION
best.model <- names(which.max(avg.cost.c))

# in case of tie, use prediction of best
the.response <- data.frame(pred)
the.means    <- rowMeans(the.response)
m.idx        <- which(the.means == 0.5)

# get final predictions
final.results        <- the.means
final.results[m.idx] <- the.response[m.idx, best.model]
final.results        <- round(final.results)

################################################################################
# POST-PROCESSING

# predict return for all cases where item_price = 0 
# (based of our internal research)

zero.idx <- which(class.price == 0)
final.results[zero.idx] <- 1