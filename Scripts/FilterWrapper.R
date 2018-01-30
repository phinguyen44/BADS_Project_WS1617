

################################################################################
# 
#   Filter for subset selection (pre-processing)
#
################################################################################
# Description:
# Filter a subset of variables from entire variable set based on information
### Filter criteria: WOE information value and Fisher score
# Backward-selection wrapper based on Random Forest

################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("tidyverse", "dplyr", "caret", "InformationValue",
                    "klaR", "Hmisc", "mlr")
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

load("BADS_WS1718_known_var.RData")

############################################################################
############################################################################
### Filtering based on information criteria ################################

#TODO: Put Fisher score function in Helpful source file
dat.input1$return <- as.factor(dat.input1$return)


### A) Numeric variables
getFisherscore <- function(variable, return){
                  bothMeans   = tapply(variable, return, mean)
                  featureMean = mean(variable)
                  diff.pos.sq = (bothMeans[1] - featureMean)^2
                  diff.neg.sq = (bothMeans[2] - featureMean)^2
                  nominator   = diff.pos.sq+ diff.neg.sq
                  bothSigma   = tapply(variable, return, sd)
                  denominator = (bothSigma[1])^2 + (bothSigma[2])^2
    coefficient <- as.numeric(nominator / denominator)
    return(coefficient)
}

# Extract fisher score for all categorical variables
allFisherscores <- apply(dat.input1[,sapply(dat.input1, is.numeric)], 
                       2, getFisherscore, dat.input1$return)

# Examine Fisher scores
allFisherscores[order(allFisherscores)]

# Find variables with high correlation
Nums.dat <- cbind(dat.input1[, sapply(dat.input1, class) == "numeric"],
                  dat.input1[, sapply(dat.input1, class) == "integer"])
Pcorr <- rcorr(as.matrix(Nums.dat), type="pearson")
Pcorr$r
rm(Nums.dat)

#Remove variables with low Fisher score and high correlation (redundant)
# discount.abs

# Information value based on WOE
woe.scores <- woe(return ~ ., data = dat.input1, zeroadj = 1)
woe.filter <- woe.scores$IV

# Check out WOE close to 0.2 (print up to 0.4)
low.woe.idx <- which(woe.scores$IV <= 0.04)
woe.filter[low.woe.idx]


# Remove variable with low information value or redundancy
dat.input1 <- dat.input1 %>% 
    dplyr::select( - discount.abs, -user_state, -user_title,
                   - order_month, - weekday, - order_year,
                   - item.color.group, -order.same.itemD, 
                   - item.basket.size.diffD, - first.order,
                   - item.basket.same.categoryD, - item.basket.category.size.diffD,
                   - WestGerm, - age.group, -brand.cluster,
                   - age.NA, - income.bl, - item_color, price.inc.ratio)
#, - basket.big)

# -user.total.items, item.basket.size.diff
# -order.same.item

############################################################################
############################################################################
### Wrapper based on backward selection#####################################

# Create a random, stratified test and training set 
set.seed(123)
part.ind <- createDataPartition(y = dat.input1$return, p = 0.67, list = FALSE) 
Test <-  dat.input1[-part.ind , ]
Train <- dat.input1[part.ind , ] 

# To prevent from overfitting, calculate relational data only on train set

### Recalculate relational data of customer
Train  <- Train %>% 
    group_by(user_id) %>% 
    dplyr::mutate(
        user.total.expen = sum(item_price),
        user.total.items = n())

# Summarise customer data
Cust.data <- Train %>% 
    group_by(user_id) %>% 
    dplyr::summarise(
        user.total.expen = sum(item_price),
        user.total.items = n())

# Assign information to Test set
Test  <- Test[,-c(23,24)]
Test  <- left_join(Test, Cust.data, by = "user_id")
Test$user.total.expen <- ifelse(is.na(Test$user.total.expen), 0, Test$user.total.expen)
Test$user.total.items <- ifelse(is.na(Test$user.total.items), 0, Test$user.total.items)
rm(Cust.data)


# Standardize both data sets 

Train.dat <- data.frame(cbind(scale(Train[, sapply(dat.input1, class) == "numeric"]),
                              scale(Train[, sapply(dat.input1, class) == "integer"]),
                                    Train[, sapply(dat.input1, class) == "factor"]))

Test.dat <- data.frame(cbind(scale(Test[, sapply(dat.input1, class) == "numeric"]),
                             scale(Test[, sapply(dat.input1, class) == "integer"]),
                                   Test[, sapply(dat.input1, class) == "factor"]))

# Calculate WOE for Train and project onto Test (exclude dummy variables)
WOE.scores  <- woe(return ~ ., data = Train.dat[,-c(23,28)], zeroadj = 1)
Train.final <- data.frame(cbind(WOE.scores$xnew, 
                                return = Train$return,
                                Train.dat[,c(23,28)]))
Test.final  <- predict(WOE.scores, newdata = Test.dat, replace = TRUE)

# Set up Data set for Wrapper and clean up
Model.dat   <- rbind(Train.final, Test.final) #merge both data sets for mlr
rm(Train.dat, Train.final, Test.dat, Test.final)

# Create task for backward selection
Selection.Task <- makeClassifTask(data = Model.dat[,], target = "return", positive = "1")
RandomForest   <- makeLearner("classif.randomForest", 
                  predict.type = "prob", 
                  par.vals = list("replace" = TRUE, "importance" = FALSE))

# Selection control for sequential backward search
SearchCtrl <- makeFeatSelControlSequential(method = "sfbs", alpha = 0.001,
                                           beta = -0.0001) 

# Indicate training and test set
rin <- makeFixedHoldoutInstance(train.inds = 1:67001, 
                                 test.inds = 67002:100000, 
                                      size = 100000)
# Sanity check of Training and Test set
rin

# Feature selection 
Selection <- selectFeatures(RandomForest, task = Selection.Task, resampling = rin,
                                   control = SearchCtrl, measures = mlr::auc,
                                   show.info = TRUE)
# Extract the selected variables
(selected.features <- Selection$x)

# Replace WOE with orginal variables
selected.features[9]   <- "item_id"
selected.features[10]  <- "item_size"
selected.features[11]  <-  "user_id"


# Define final data set
dat.ready <- dat.input1[,selected.features]

# Export final data set
save(dat.ready, file = "BADS_WS1718_known_ready.RData" )

