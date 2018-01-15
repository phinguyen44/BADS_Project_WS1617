
################################################################################
# 
#   Logistic Regression
#
################################################################################
# Description:
#  - creates log regression baseline model
#  - N-fold crossvaliadation
#  - variable selection via mix of filtering & wrappers
################################################################################

load("BADS_WS1718_known_var.RData")

library("caret") 
library(hmeasure)
library(InformationValue)
library(mlr)


# Create partition
set.seed(321)
Train <- createDataPartition(y = dat.input2$return, p = 0.7, list = FALSE)
Test <-  dat.input2[-Train, ] 
Train <- dat.input2[Train, ]
estimates <- list()

# Preliminary filtering: Remove not useful variables

vec.remove <- c("item_size", "order_date", "delivery_date",
               "item_id", "item_color", "user_id", "user_dob",
               "user_reg_date", "user_dob_year", "WOE.brand",
               "WOE.size", "aver.return.item", "WOE.item",
               "brand_id", "order_year", "aver.return.brand", 
               "no.return")

idx.remove <- which(colnames(Train) %in% vec.remove)

Train <- Train[,-idx.remove]

# Baseline Logistic regression
logReg1 <- glm(return ~  item_price +  user_title +  user_state  +      
                 + deliver.time +  order_month +   weekday + age1  , 
               data = Train, family = binomial(link = "logit"))

# Wrapper: step-wise backward 


# Filtering: Eliminate variables based on coefficients
summary(logReg)

# Make predictions
estimates[["logReg1"]] <- predict(logReg1, newdata = Test, type = "response")

estimates[["m.est"]] <- (estimates$logReg1 + estimates$logReg2 + estimates$logReg3 + 
                         estimates$logReg4 + estimates$logReg5) / 5


# Check out model performance 
estimates.df <- data.frame(estimates)  
AUC <- HMeasure(as.numeric(Test$return)-1, estimates.df) 
auc_logReg <- AUC$metrics['AUC']
auc_logReg

misClassError(Test$return, estimates$m.est, threshold = 0.5)
sensitivity(Test$return, estimates$m.est, threshold = 0.5)
specificity(Test$return, estimates$m.est, threshold = 0.5)

plotROC(Test$return, estimates$logReg)

BrierScore <- function(y, predicted){
    sum((y - predicted)^2) / length(y)}

BrierScore(Test$return, estimates$logReg)

# Determine optimal cutoff threshold
optimalCutoff(actuals = Test$return, predictedScores = estimates$logReg, 
              optimiseFor = "Zeros")


