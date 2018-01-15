
################################################################################
# 
#   Data Cleaning
#
################################################################################
# Description:
# Imputation of delivery time and age according to
# different imputation methods
################################################################################

# Load pre-cleaned data set 

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

    # LOAD NECESSARY PACKAGES & DATA
    # List all packages needed for session
    neededPackages <- c("ggplot2", "mice", "VIM")
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
    
    # Load dataset
    dat.input <- load("BADS_WS1718_known_clean.RData")
    load("BADS_WS1718_known_clean.RData")
    names.vec <- colnames(dat.input)

############################################################################

### Imputation of age and delivery

# Step 1: Analyze distribution of age & delivery time
    
    summary(dat.input$age)
    
    # Plot probability density
    
    ggplot(dat.input, aes(x=age)) +
        geom_histogram(binwidth=.5, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(age, na.rm=T)), 
                   color="red", linetype="dashed", size=1) 
    
    summary(dat.input$deliver.time)
    
    # Plot probability density
    ggplot(dat.input, aes(x=deliver.time)) +
        geom_histogram(binwidth=.5, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(deliver.time, na.rm=T)), 
                   color="red", linetype="dashed", size=1) 
    

    # Not normally distributed, rather log normal
    # Outliers can be a problem
    
# Step 2: Analyze NA pattern
    md.pattern(dat.input)
    
    aggr(dat.input, col=c('navyblue','green'),
                        numbers=TRUE, sortVars=TRUE,
                        labels=names(dat.input), cex.axis=.4,
                        gap=3, ylab=c("Missing data","Pattern"))

    # Age and delivery time do not seem to be missing jointly
    
############################################################################
### Method 1: Based on median
  
    dat.input1 = dat.input
      
    mage = median(dat.input1$age, na.rm = TRUE)
    
    mdelivertime = median(dat.input1$deliver.time, na.rm = TRUE)
    
    age.na = is.na(dat.input1$age)
    
    dat.input1$age[age.na] <- mage
    
    dtime.na = is.na(dat.input1$deliver.time)
    
    dat.input1$deliver.time[dtime.na] <- mdelivertime
    
# Check new distributions   
    
    summary(dat.input1$age)
    
    # Plot probability density
    
    ggplot(dat.input1, aes(x=age)) +
        geom_histogram(binwidth=.5, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(age, na.rm=T)), 
                   color="red", linetype="dashed", size=1) 
    
    summary(dat.input1$deliver.time)
    
    # Plot probability density
    ggplot(dat.input1, aes(x=deliver.time)) +
        geom_histogram(binwidth=.5, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(deliver.time, na.rm=T)), 
                   color="red", linetype="dashed", size=1) 
    
    
############################################################################
# Export imputed data set 1

save(dat.input1, file = "BADS_WS1718_known_imp1.RData" )
 
############################################################################
### Method 2: Based on Maximum Likelihood
## Assumption: Data missing at completely at random

dat.input2 = dat.input

# Selection of suitable variables for age imputation: user_title, brand_id, item_size, item_price

# Attention: only run when necessary, take a lot of time
age.imp <- mice(dat.input2[, c("age", "user_title", "brand_id", "item_size", 
                           "item_price")],m=5,maxit=50,meth='pmm',seed=123)
m <- 5
for(i in 1:m){
    completeData[i] <- complete(age.imp, m)
}

save(completedData1, completedData2, completedData3, 
     completedData4, completedData5, 
     file = "BADS_WS1718_known_imp2.RData")

############################################################################
### Method 3: Drop missing observation

dat.input3 <- dat.input

dat.input3 <- na.omit(dat.input3) 

# Export data set 3 without NA´s
    
save(dat.input3, file = "BADS_WS1718_known_imp3.RData" )
    
    