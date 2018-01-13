################################################################################
# XGBoost.R
#
################################################################################
# Description:
# BADS project - predict using gradient boosting 
# (with platt scaling?)
# 
# 
# Notes: how does it handle non-linear relationships?
# Cross-validation of model? Can you?
# Metaparameter tuning?
# Include motivation / advantages of gradient boosted trees
# Can it accept a custom loss function? Or does it generate probabilities and we choose the cutoff
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, caret, mlr, xgboost, mice, pROC)

# Load data
df.train <- load("Data/BADS_WS1718_known_imp1.RData")
df.test  <- read.csv("Data/BADS_WS1718_class.csv")
