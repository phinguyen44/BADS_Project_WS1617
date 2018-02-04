

################################################################################
# 
#   Preliminary Parameter Tuning
#
################################################################################
# Description:
# Find suitable subset for Hyperparameter for final tuning
################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("tidyverse", "dplyr", "InformationValue",
                    "klaR", "Hmisc", "mlr", "xgboost", "caret")
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

# source helper packages
source("Scripts/Helpful.R")

load("BADS_WS1718_known_ready.RData")
############################################################################
############################################################################
### Random Forest

# Create a random, stratified test and training set 
set.seed(123)
part.ind <- createDataPartition(y = dat.ready$return, p = 0.67, list = FALSE) 
Test <-  dat.ready[-part.ind , ]
Train <- dat.ready[part.ind , ] 

# To prevent from overfitting, calculate relational data only on train set
# Summarise customer data based on training set
Cust.data <- Train %>% 
    group_by(user_id) %>% 
    dplyr::summarise(
        user.total.expen = sum(item_price),
        user.total.items = n())

# Assign information to Training and Test set
Test  <- Test[,-c(6,10)]
Train <- Train[,-c(6,10)]
Test  <- left_join(Test, Cust.data, by = "user_id")
Train  <- left_join(Train, Cust.data, by = "user_id")

Test$user.total.expen <- ifelse(is.na(Test$user.total.expen), 0, Test$user.total.expen)
Test$user.total.items <- ifelse(is.na(Test$user.total.items), 0, Test$user.total.items)
rm(Cust.data)
# Standardize both data sets 

Train.dat <- data.frame(cbind(scale(Train[, sapply(Train, class) == "numeric"]),
                              scale(Train[, sapply(Train, class) == "integer"]),
                              Train[, sapply(Train, class) == "factor"]))

Test.dat <- data.frame(cbind(scale(Test[, sapply(Test, class) == "numeric"]),
                             scale(Test[, sapply(Test, class) == "integer"]),
                             Test[, sapply(Test, class) == "factor"]))

# Calculate WOE for Train and project onto Test (exclude dummy variables)
WOE.scores  <- woe(return ~ ., data = Train.dat[,-c(4,14)], zeroadj = 1)
Train.final <- data.frame(cbind(WOE.scores$xnew, 
                                return = Train$return,
                                Train.dat[,c(4,14)]))
Test.final  <- predict(WOE.scores, newdata = Test.dat, replace = TRUE)

# Set up Data set for Wrapper and clean up
Model.dat   <- rbind(Train.final, Test.final) #merge both data sets for mlr
rm(Train.dat, Train.final, Test.dat, Test.final)

task <- makeClassifTask(data = Model.dat, target = "return", positive = "1")
task

rf <- makeLearner("classif.randomForest", 
predict.type = "prob", # prediction type needs to be specified for the learner 
par.vals = list("replace" = TRUE, "importance" = FALSE))
rf

## Tuning and Hyperparameter setting
rf.parms <- makeParamSet(
    makeDiscreteParam("ntree", values = seq(100, 500, by=100)),
    makeIntegerParam("nodesize", lower = 1, upper = 10),
    makeIntegerParam("mtry", lower = 1, upper = 10)
) 
# Density of parameters
tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)

# Indicate training and test set
rin <- makeFixedHoldoutInstance(train.inds = 1:67001, 
                                test.inds = 67002:100000, 
                                size = 100000)
# Sanity check of Training and Test set
rin

# Set up tuning 
timing <- list()
timing[["simple"]] <- system.time(
tuning <- tuneParams(rf, task = task, resampling = rin,
par.set = rf.parms, control = tuneControl, measures = mlr::acc)
)
# Check out optimal parameter values

tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)

############################################################################
############################################################################
### Gradient boosting 

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", par.vals = list("verbose" = 1)) 

# xgb tuning parameters
xgb.parms <- makeParamSet(
    makeDiscreteParam("booster", values = c("gbtree")),
    makeDiscreteParam("gamma", values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)),
    makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)),
    makeDiscreteParam("nrounds", values = c(50, 100, 200, 400, 600)),
    makeDiscreteParam("lambda", values = seq(0.3, 1, by=0.05)),
    makeIntegerParam("max_depth", lower = 3L, upper = 8L))

xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rin,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::acc)

# Optimal parameters 
xgb.tuning$x
