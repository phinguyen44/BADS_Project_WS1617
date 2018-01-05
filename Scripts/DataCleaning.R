
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

    # LOAD NECESSARY PACKAGES & DATA
    # List all packages needed for session
    neededPackages <- c("dplyr", "tidyr", "ggplot2", "lubridate", 
                       "magrittr","infuser", "foreign")
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
    
    dat.input <- read.csv("BADS_WS1718_known.csv", sep = ",", header <- TRUE)
    names.vec <- colnames(dat.input)

############################################################################

### Delivery time & not returned
    
    # Declare missing value in delivery date as such
    delivery_date1 <- ifelse(dat.input$delivery_date == "?", NA, TRUE)
    dat.input$delivery_date <- dat.input$delivery_date[delivery_date1]
    
    missing <- is.na(dat.input$delivery_date)
    no.return <- ifelse(missing, 1, 0)
    
    # If delivery date is NA, item is not returned
    mean(dat.input[missing,]$return)
    
    #mark unplausible dates (31.12.1990) as NA
    delivery_date2 <- ifelse(dat.input$delivery_date == "1990-12-31", NA, TRUE)
    dat.input$delivery_date <- dat.input$delivery_date[delivery_date2]
    
    # Calculate delivery time
    mydate2 <- as.Date(dat.input[, 2])
    mydate3 <- as.Date(dat.input[, 3])
    
    diff.in.days = data.frame(
        difftime(mydate3, mydate2, units = "days"))
    
    dat.input  <- cbind(dat.input, diff.in.days[,1], no.return)
    colnames(dat.input) <- paste0(c(names.vec, "delivery.time", "no.return"))
    
    rm(mydate2, mydate3, delivery_date1, delivery_date2, diff.in.days, missing, no.return)
    
    
### Month of order
    
    # Calculate order month
    
    order.month <- as.Date(dat.input$order_date)
    z <- function(x){substr(x, 6, 7)}
    
    dat.input$order_month<- z(order.month)
    rm(z, order.month)
    
### Weekday of order

# Note: April 1 in 2012 was a Sunday   
    
    # Aggregate over mean - dates get ordered automatically
    return.daily = aggregate(x = dat.input$return, by = list(dat.input$order_date), mean)
    
    return.daily$weekday = paste0(c(rep(c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"), 52), "Sun"))
    
    colnames(return.daily) = c("order_date", "mean.return", "weekday" )
    
    dat.input = merge(dat.input, return.daily[,c(1,3)], by = "order_date")
    
    rm(return.daily)
    
############################################################################

### User data of birth and age
    
    # Declare missing values in dob as such
    missing2           <- ifelse(dat.input$user_dob == "?", NA, TRUE)
    dat.input$user_dob <- dat.input$user_dob[missing2]
    
    rm(missing2)
    
    # Create birth year variable
    date.dob <- as.Date(dat.input$user_dob)
    year.func <- function(x){substr(x, 1, 4)}
    
    dat.input$user_dob_year <- year.func(date.dob)
    dat.input$user_dob_year <- as.numeric(dat.input$user_dob_year)
    rm(date.dob, year.func)
 
### Calculate Age of customer
    # Note: Age is age at the year of purchase
    
    # Extract year of order
    year = function(x){substr(x, 1, 4)}
    dat.input$order_year <- year(dat.input$order_date)
    dat.input$order_year <- as.numeric(dat.input$order_year)
    
    # Calculate age of customer
    dat.input$age <- dat.input$order_year - dat.input$user_dob_year
    
    rm(year)
    
    # Mark unplausible age entries as NA
    # Remove age above 100
    z <- ifelse(dat.input$age >= 100, NA, TRUE)
    dat.input$age <- dat.input$age[z]
    rm(z)
    
    z <- ifelse(dat.input$age < 12, NA, TRUE)
    dat.input$age <- dat.input$age[z]
    rm(z)
    
############################################################################
    
colnames(dat.input) = c("order_item_id" ,                            
                       "order_date"    ,                            
                       "delivery_date" ,                           
                       "item_id"       ,                          
                       "item_size"     ,                         
                       "item_color"    ,                        
                       "brand_id"      ,                        
                       "item_price"    ,                           
                       "user_id"       ,                           
                       "user_title"    ,                            
                       "user_dob"      ,                            
                       "user_state"    ,                            
                       "user_reg_date" ,                            
                       "return",
                       "deliver.time",
                       "no.return",
                       "order_month",
                       "weekday",
                       "user_dob_year",
                       "order_year",
                        "age")
    
############################################################################
### Format time variables
    
    # Declare all date variables as correct data type

    dat.input$order_date    <- as.Date(dat.input$order_date)
    dat.input$delivery_date <- as.Date(dat.input$delivery_date)
    dat.input$user_reg_date <- as.Date(dat.input$user_reg_date)
    dat.input$order_month   <- as.factor(dat.input$order_month)
    dat.input$deliver.time  <- as.numeric(dat.input$deliver.time)
    
    
    
############################################################################
# Export clean data set

save(dat.input, file = "BADS_WS1718_known_clean" )
    

       