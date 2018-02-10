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
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

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

load("Data/BADS_WS1718_known_var.RData")

############################################################################
############################################################################
### Filtering based on information criteria ################################

#TODO: Put Fisher score function in Helpful source file
dat.input1$return <- as.factor(dat.input1$return)


### A) Numeric variables
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

#Remove variables with low Fisher score  
# Check high correlation (redundant)
# discount.abs

# Information value based on WOE
woe.scores <- woe(return ~ ., data = dat.input1, zeroadj = 1)
woe.filter <- woe.scores$IV

# Check out WOE close to 0.2 (print up to 0.4)
low.woe.idx <- which(woe.scores$IV <= 0.04)
woe.filter[low.woe.idx]


# Remove variable with low information value
dat.input1 <- dat.input1 %>% 
    dplyr::select( - discount.abs, -user_state, -user_title,
                   - order_month, - weekday, - order_year,
                   - item.color.group, -order.same.itemD, 
                   - item.basket.size.diffD, - first.order,
                   - item.basket.same.categoryD, - item.basket.category.size.diffD,
                   - WestGerm, - age.group, - brand.cluster, - basket.big
                   - age.NA, - income.bl, - item_color, - price.inc.ratio,
                   - income.age, - order.same.item)

############################################################################
############################################################################
### Wrapper based on backward selection#####################################

# Create a random, stratified test and training set 
set.seed(123)
part.ind <- createDataPartition(y = dat.input1$return, p = 0.67, list = FALSE) 
Test <-  dat.input1[-part.ind , ]
Train <- dat.input1[part.ind , ] 

# To prevent from overfitting, calculate relational data only on train set

# Summarise customer data based on training set
Cust.data <- Train %>% 
    group_by(user_id) %>% 
    dplyr::summarise(
        user.total.expen = sum(item_price),
        user.total.items = n())

# Assign information to Training and Test set
Test  <- Test[,-c(23,24)]
Train <- Train[,-c(23,24)]
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
WOE.scores  <- woe(return ~ ., data = Train.dat[,-c(21,26)], zeroadj = 1)
Train.final <- data.frame(cbind(WOE.scores$xnew, 
                                return = Train$return,
                                Train.dat[,c(21,26)]))
Test.final  <- predict(WOE.scores, newdata = Test.dat, replace = TRUE)

# Set up Data set for Wrapper and clean up
Model.dat   <- rbind(Train.final, Test.final) #merge both data sets for mlr
rm(Train.dat, Train.final, Test.dat, Test.final)

# Create task for backward selection
Selection.Task <- makeClassifTask(data = Model.dat[,], target = "return", positive = "1")
RandomForest   <- makeLearner("classif.randomForest", 
                  predict.type = "prob", 
                  par.vals = list("replace" = TRUE, "importance" = FALSE))

# Selection control for Sequential floating backward selection (SFBS)
#Alpha: Lower bound for upward step (increase in AUC required)
#Beta: Lower bound for downward step (if negative, slight decrease in AUC allowed)
SearchCtrl <- makeFeatSelControlSequential(method = "sfbs", alpha = 0.01,
                                           beta = - 0.001) 

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
(Selection$x)

# Replace WOE with orginal variables & add return
vec.FeatSelection <- Selection$x
vec.FeatSelection[11]   <- "brand_id"
vec.FeatSelection[12]   <- "item_id"
vec.FeatSelection[13]   <- "item_size"
vec.FeatSelection[14]   <- "user_id"
vec.FeatSelection[15]   <- "basket_size"
vec.FeatSelection[18]   <- "return"

# Define final data set
dat.ready <- dat.input1[,vec.FeatSelection]

# Alternatively, if you cannot run wrapper again
dat.ready <- dat.input1 %>% 
    dplyr::select(  age, item_price, deliver.time,
                    no.return, account.age.order, user.total.expen,
                    item.basket.size.same, item.basket.size.diff,
                    item.basket.same.category, user.total.items,
                    brand_id, item_id, item_size, user_id,
                    basket.size, basket.big, discount.pc, return)

# Export final data set
save(dat.ready, file = "BADS_WS1718_known_ready.RData" )

