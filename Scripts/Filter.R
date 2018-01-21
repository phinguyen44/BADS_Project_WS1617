

################################################################################
# 
#   Filter for subset selection (pre-processing)
#
################################################################################
# Description:
# Filter a subset of variables from entire variable set based on information
# criteria (WOE information value and Fisher score)
# Reduce dimensionality and prevent overfitting
################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("tidyverse", "dplyr", "caret", "InformationValue",
                    "klaR")
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

getFisherscore <- function(variable, return){
                   Mean = tapply(variable, return, mean)
                  sigma = tapply(variable, return, sd)
             difference = abs(diff(Mean))
    coefficient <- as.numeric(difference / sqrt(sum(sigma^2)))
    return(coefficient)
}

# Extract fisher score for all categorical variables
allFisherscores <- apply(dat.input1[,sapply(dat.input1, is.numeric)], 
                       2, Score.fisher, dat.input1$return)

# Examine Fisher scores
allFisherscores

# Information value based on WOE
woe.scores <- woe(return ~ ., data = dat.input1, zeroadj = 1)
woe.filter <- woe.scores$IV
low.woe.idx <- which(woe.scores$IV <= 0.01)
woe.filter[low.woe.idx]
# low WOE for: user_state, user_title, order_year, weekday, 
#item.color.group, item.basket.size.diffD, order_year, oder.same.itemD
#first.order , WestGerm, brand.cluster


# Remove variable with low information value
dat.input1 <- dat.input1 %>% 
    dplyr::select(
#        -user_dob, -ord)

# Export final data set
save(dat.input1, file = "BADS_WS1718_known_var.RData" )


