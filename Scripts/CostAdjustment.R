################################################################################
# 
#   Costs.df optimization (post-processing)
#
################################################################################
# Description:
# Analyze Costs.df asymmetry of misclassification
# Final lower and upper threshold in terms of price for adopting pure prediction
# strategy (always predict return / not return)
################################################################################
rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("tidyverse", "dplyr", "InformationValue",
                     "Hmisc", "mlr", "caret", "klaR")
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
### Use baseline model (LogReg) for predicitions ###########################

# Create a random, stratified test and training set 
set.seed(123)
dat.ready$return <- as.factor(dat.ready$return)
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

# Adjust user information for new levels
Test$user.total.expen <- ifelse(is.na(Test$user.total.expen), 0, Test$user.total.expen)
Test$user.total.items <- ifelse(is.na(Test$user.total.items), 0, Test$user.total.items)
rm(Cust.data)

# Calculate WOE for Train and project onto Test (exclude dummy variables)
WOE.scores  <- woe(return ~ ., data = Train[,-c(4,14)], zeroadj = 1)
Train.final <- data.frame(cbind(WOE.scores$xnew, 
                                return = Train$return,
                                Train[,c(4,14)]))
Test.final  <- predict(WOE.scores, newdata = Test, replace = TRUE)

# Run a simple log Reg and predict for Test set
logReg <- glm(return ~. , 
              data = Train.final, family = binomial(link = "logit"))

estimates <- predict(logReg, newdata = Test.final, type = "response", 
                     replace = TRUE)

# Sanity check of model estimation
misClassError(Test.final$return, estimates, threshold = 0.5)

############################################################################
### Compare model prediction and pure strategies ###########################

# Create dataframe for model predictions and Costs.df
Costs.df <- cbind(Test.final, log.pred = estimates)
Costs.df$item_price <- Test$item_price # assign non-scaled item price 

# Use 0.5 threshold for prediction
Costs.df$pred <- ifelse(Costs.df$log.pred <= 0.5, 0, 1)

# Calculate number of true positives (TP) and negatives (TN) and
# false negatives (FN) and positives (FN)
Costs.df$TN <- ifelse(Costs.df$return == 0 & Costs.df$pred == 0, 1, 0)
Costs.df$FP <- ifelse(Costs.df$return == 0 & Costs.df$pred == 1, 1, 0)
Costs.df$FN <- ifelse(Costs.df$return == 1 & Costs.df$pred == 0, 1, 0)
Costs.df$TP <- ifelse(Costs.df$return == 1 & Costs.df$pred == 1, 1, 0)

# Calculate error Costs.df ratio (error Costs.df / item price)
Costs.df$error1r <- (Costs.df$item_price*0.5)/Costs.df$item_price
Costs.df$error2r <- (Costs.df$item_price*0.25 + 7.5)/Costs.df$item_price

# Plot cost ratios
pplot <- ggplot(data=Costs.df, aes(x=item_price))
pplot + geom_line(aes(y=Costs.df$error1r)) + geom_line(aes(y=Costs.df$error2r))

###########################################################
# Find optimal lower cut-off

# price bins (discrete)
price  <- num.check(Costs.df, "item_price")
priceB <- discrete.bin(price, numbins = 40) #results in 53 bins due to multispan

# assign bins (manually add 0)
Costs.df$item_priceB  <- assign.bins(Costs.df, priceB, "item_price")
Costs.df$item_priceB  <- as.character(Costs.df$item_priceB)
idx <- which(Costs.df$item_price == 0)
Costs.df$item_priceB[idx] <- "[0,0]"
Costs.df$item_priceB  <- as.factor(Costs.df$item_priceB)

# Determine lower cut-off
compare <- Costs.df %>% 
    group_by(item_priceB) %>% 
    dplyr::summarise(
        av.return = mean(return),
        av.price  = mean(item_price),
        cost.err1 = 0.5*av.price,
        cost.err2 = ((7.5 + 0.25*av.price)*1),
        spread.cost = (cost.err2)/(cost.err1+0.0001), #adjust due to zero division
        FN.numb    = sum(FN),
        TN.numb    = sum(TN),
        ratio      = (TN.numb+ 0.0001)/(FN.numb + 0.0001),
        cutoff     =  spread.cost - ratio)


pplot <- ggplot(data=compare, aes(x=av.price))
# add two line layers
pplot + geom_line(aes(y=ratio))



# Determine higher cut-off
compare2 <- Costs.df %>% 
    group_by(item_priceB) %>% 
    dplyr::summarise(
        av.return = mean(return),
        av.price  = mean(item_price),
        cost.err1 = 0.5*av.price,
        cost.err2 = ((7.5 + 0.25*av.price)*1),
        spread.cost = (cost.err2)/(cost.err1+0.0001), #adjust due to zero dvision
        FP.numb    = sum(FP),
        TP.numb    = sum(TP),
        ratio      = (FP.numb+ 0.0001)/(TP.numb + 0.0001),
        cutoff     =  spread.cost - ratio)

pplot <- ggplot(data=compare2, aes(x=av.price))
# add two line layers
pplot + geom_line(aes(y=ratio))


###########################################################
