################################################################################
# TrainFinalModel.R
#
################################################################################
# Description:
# 
# BADS project - train final model
#
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
# load("Data/BADS_WS1718_class_ready.RData")
load("Data/step4.RData")
dat.test <- dat.ready
load("Data/BADS_WS1718_known_ready.RData")
load("Data/CalibratedThreshold-Phi.Rdata")
truedata <- read.csv("Data/BADS_WS1718_class_20180115.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# INITIAL SETUP

# SAVE DATA FOR LATER
df.label <- dat.ready$return
df.price <- dat.ready$item_price

class.price <- dat.test$item_price
class.id    <- dat.test$order_item_id

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
        item_price)

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

# TRAIN MODEL
fin   <- map2(mods, learners, function(f, x) f(x, 
                                               df.train, 
                                               df.class, 
                                               calib = TRUE, 
                                               final = TRUE))

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

# merge results with user id
final.df <- data.frame(orderid = class.id, prediction = final.results)

save(final.df, file = "Data/FINALPREDICTIONS.RData")