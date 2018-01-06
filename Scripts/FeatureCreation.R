
################################################################################
# 
#   Data Cleaning
#
################################################################################
# Description:
# Cleaning and formatting of variables
# Creation and transformation of some basic variables 
#
################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

    # LOAD NECESSARY PACKAGES & DATA
    # List all packages needed for session
    neededPackages <- c()
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

############################################################################

### Brand-id: Use Weight of evidence

#  Calculate mean and count for brand return and sale
agg=  aggregate(dat.input$return, list(dat.input$brand_id), 
                          FUN = function(x)c(mn = mean(x), n = length(x), 
                                good = mean(x)*length(x), bad = (1-mean(x))*length(x)))
    
brand_agg <- do.call(data.frame, agg)

colnames(brand_agg) = c("brand_id", "aver.return.brand", "sales.brand", "good", "bad")

rm(agg)

# Check distribution of sales and return

boxplot(brand_agg$sales.brand, horizontal=TRUE,axes=TRUE,outline=FALSE)
summary(brand_agg$sales.brand)

# Function of Weight of Evidence

GOOD = sum(brand_agg$good)
BAD  = sum(brand_agg$bad)

brand_agg$WOE = log((brand_agg$bad/BAD)/(brand_agg$good/GOOD))

### Improve measure according to Zdravevski (2010)

# Case 1: No cases in good and bad: Not existent

# Case 2: Either no cases in good or bad: WOE -> Inf
# Replace occurence (=0) with 1

brand_agg$good = ifelse(brand_agg$good == 0, 1, brand_agg$good)
brand_agg$bad = ifelse(brand_agg$bad == 0, 1, brand_agg$bad)    

# Redo Calculation of WOE
brand_agg$WOE = log((brand_agg$bad/BAD)/(brand_agg$good/GOOD))

# Calculate Information Value

brand_agg$InVa = ((brand_agg$bad/BAD) - (brand_agg$good/GOOD)) * brand_agg$WOE

# Overall IV = 0.14 medium predictor 
sum(brand_agg$InVa)


############################################################################
# Merge created variables

vec = c("brand_id", "aver.return.brand", "sales.brand", "WOE")

dat.input = merge(dat.input, brand_agg[, vec], by = "brand_id" )
    
### Sort by order id again
dat.input = dat.input[order(dat.input$order_item_id),]

############################################################################
# Export clean data set

save(dat.input, file = "BADS_WS1718_known_var.RData" )
    

       