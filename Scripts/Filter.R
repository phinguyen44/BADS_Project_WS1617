

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
                    "klaR", "Hmisc")
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
Pcorr1 <- rcorr(as.matrix(Nums.dat), type="pearson")
Pcorr1$r
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
                   - item.basket.size.diff, - order.same.item,
                   - basket.big)

dat.ready <- dat.input1

# Export final data set
save(dat.ready, file = "BADS_WS1718_known_ready.RData" )


