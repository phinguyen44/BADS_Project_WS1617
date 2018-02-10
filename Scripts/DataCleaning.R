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
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21/Data")
setwd(wd)

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("dplyr", "tidyr", "lubridate", 
                    "foreign")
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
dat.input <- read.csv("BADS_WS1718_known.csv", sep = ",", header = TRUE)

# source helper packages
source("Scripts/Helpful.R")

############################################################################

### Delivery time & not returned
    
# Flag missing value in delivery date as such
IND.delivery_date       <- ifelse(dat.input$delivery_date == "?", NA, TRUE)
dat.input$delivery_date <- dat.input$delivery_date[IND.delivery_date]

IND.missing         <- is.na(dat.input$delivery_date)
dat.input$no.return <- ifelse(IND.missing, 1, 0)

# If delivery date is NA, item is not returned
mean(dat.input[IND.missing,]$return)

#Mark unplausible dates (31.12.1990) as NA
IND.deliver             <- ifelse(dat.input$delivery_date == "1990-12-31", NA, TRUE)
dat.input$delivery_date <- dat.input$delivery_date[IND.deliver]

# Calculate delivery time
dat.input$deliver.time <- difftime(as.Date(dat.input[, 3]), 
                                   as.Date(dat.input[, 2]), 
                                           units = "days")
rm(IND.deliver, IND.missing)
    
### Month and weekday of order
dat.input <- dat.input %>% 
    dplyr::mutate(
        order_month   = factor(months(ymd(order_date))),
        order_weekday = factor(weekdays(ymd(order_date))))
    
############################################################################

### User data of birth and age

# Declare missing values in dob as such
IND.missing.dob    <- ifelse(dat.input$user_dob == "?", NA, TRUE)
dat.input$user_dob <- dat.input$user_dob[IND.missing.dob]

rm(IND.missing.dob)

# Create birth year variable, year of order, age of customer and flag missings
dat.input <- dat.input %>% 
    dplyr::mutate(
        user_dob_year = as.numeric(year.extraction(as.Date(user_dob))),
        order_year    = as.numeric(year.extraction(order_date)),
        age           = order_year - user_dob_year,
        age.NA        = ifelse(is.na(age), 1, 0))

# Note: Age is age at the year of purchase

# Mark unplausible age entries as NA
dat.input <- dat.input %>% 
    dplyr::mutate(
        inplausible.high = ifelse(age >= 100, NA, TRUE),
        inplausible.low  = ifelse(age < 16, NA, TRUE),
        age              = age[inplausible.high],
        age              = age[inplausible.low])  %>% 
    dplyr::select(- inplausible.high, - inplausible.low)

############################################################################
### Item sizes

# Convert redundant item_size
dat.input$item_size <- factor(toupper(dat.input$item_size))
    
############################################################################
### Final formatting
    
### Format time variables
    
# Declare all date variables as correct data type

dat.input <- dat.input %>% 
    dplyr::mutate(
      order_date     = as.Date(order_date),
      delivery_date  = as.Date(delivery_date),
      user_reg_date  = as.Date(user_reg_date),
      order_month    = as.factor(order_month),
      deliver.time   = as.numeric(deliver.time),
      order_year     = as.factor(order_year),
      age.NA         = as.factor(age.NA))
    
############################################################################
# Export clean data set

save(dat.input, file = "BADS_WS1718_known_clean.RData")
