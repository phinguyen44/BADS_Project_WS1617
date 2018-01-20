################################################################################
# FeatureCreation2.R
#
################################################################################
# Description:
# BADS project - different attempt at feature creation
# Remember to apply same features to final unknown set
# 
# TODO: check return rates of new variables (also plot WOE)
# TODO: consider discretizing age, item_price, and delivery time
# 
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

# Load packages
needs(tidyverse, magrittr, purrr, infuser)

# Load data
original <- read.csv("Data/BADS_WS1718_known.csv", sep = ",", header = TRUE)
load("Data/BADS_WS1718_known_imp1.RData")

# Source performance metric calculations
source("Scripts/Helpful.R")

################################################################################
# FEATURE ENGINEERING

dat.input1 <- dat.input1 %>% 
    mutate(item_id    = factor(item_id),
           brand_id   = factor(brand_id),
           user_id    = factor(user_id),
           weekday    = factor(weekday),
           order_year = factor(order_year)
           ) %>% 
    dplyr::select(order_item_id,
                  user_id, user_title, age, user_state, user_reg_date,
                  #purch, user_idWOE
                  order_date, delivery_date, deliver.time, order_year,
                  order_month, weekday,
                  item_price, item_id, brand_id, item_color, item_size,
                  no.return,
                  return)

################################################################################
# DEMOGRAPHIC LEVEL

# Total Items Purchased
# Prior Orders Made (order is assumed item by same user_id on same date)
unique.orders <- dat.input1 %>% 
    distinct(user_id, order_date) %>% 
    arrange(user_id, order_date) %>% 
    group_by(user_id) %>% 
    mutate(user_purchase_num = row_number())

dat.input1 <- dat.input1 %>% 
    left_join(unique.orders, by = c("user_id", "order_date"))

# user_purchase_num = num.check(dat.input1, "user_purchase_num")
# ggplot(data=user_purchase_num, aes(x=Var1, y=ReturnRate)) + 
#     geom_bar(stat="identity")
# discretize = discrete.bin(user_purchase_num, "user_purchase_num")

# user_id_WOE
dat.input1$user_id_WOE <- WOE(dat.input1, "user_id")

################################################################################
# BASKET LEVEL

# Order Size (order is assumed item by same user_id on same date)
dat.input1 <- dat.input1 %>% 
    group_by(user_id, order_date) %>% 
    mutate(order_size = n())

################################################################################
# PRODUCT LEVEL

# WOE item categorical variables
dat.input1$item_id_WOE    <- WOE(dat.input1, "item_id")
dat.input1$brand_id_WOE   <- WOE(dat.input1, "brand_id")
dat.input1$item_color_WOE <- WOE(dat.input1, "item_color")
dat.input1$item_size_WOE  <- WOE(dat.input1, "item_size")

# TODO: MAYBE JUST MAKE THESE DUMMY VARIABLES (yes. no?)
# TODO: How to make sure that it doesn't count the variable to be omitted?

# Number of same item in basket
dat.input1 <- dat.input1 %>% 
    group_by(user_id, order_date, item_id) %>% 
    mutate(order_same_item = n())

# Items with same color, size, different brand in same basket
dat.input1 <- dat.input1 %>% 
    group_by(user_id, order_date, item_color, item_size) %>% 
    mutate(order_same_cs = n())

# Items with same color, brand, different size in same basket
dat.input1 <- dat.input1 %>% 
    group_by(user_id, order_date, item_color, brand_id) %>% 
    mutate(order_same_cb = n())

# Items with same brand, size, different color in same basket
dat.input1 <- dat.input1 %>% 
    group_by(user_id, order_date, brand_id, item_size) %>% 
    mutate(order_same_bs = n())

dat.input2 <- data.frame(dat.input1)

################################################################################
# SELECT FINAL VARIABLES

save(dat.input2, file = "BADS_WS1718_known_feat.RData" )
