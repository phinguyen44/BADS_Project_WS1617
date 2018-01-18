
################################################################################
# 
#   Feature Creation and Extraction
#
################################################################################
# Description:
# Create all supervised and unsupervised features to be built on training set
# Feature categories:
# a) item
# b) basket/order
# c) customer
#
################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("cluster", "ggplot2", "tidyverse")
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
load("BADS_WS1718_known_imp1.RData")

############################################################################
############################################################################
### Feature creation on item level #########################################

###  A) Item price + discount ###

# Item price: Decretize price using equal binning
# @ Phi: please enter your code here, coudnt really do it
# make sure that the price of 0 is an extra category!


# Item discount: calculate discount based on max price per item & size

dat.input1  <- dat.input1 %>% 
    group_by(item_id, item_size) %>% 
    dplyr:: mutate(
        max.price.item.size = max(item_price),
        min.price.item.size = min(item_price),
        range.price.item.size = max(item_price) - min(item_price))

# Absolute discount (difference)
dat.input1$discount.abs <- dat.input1$max.price.item.size - dat.input1$item_price 

# Discount in percentage
dat.input1$discount.pc  <- (dat.input1$max.price.item.size - dat.input1$item_price)/
                            dat.input1$max.price.item.size
dat.input1$discount.pc  <- ifelse(dat.input1$discount.pc == "NaN",
                                  0, dat.input1$discount.pc )
    

# Dummy whether a customer paid max or min price
dat.input1$max.price.paid <- ifelse(dat.input1$item_price == 
                                    dat.input1$max.price.item.size, "1", "0")

dat.input1$min.price.paid <- ifelse(dat.input1$item_price == 
                                        dat.input1$min.price.item.size, "1", "0")

### B) Item Colour ###

# Create dataframe containing groups of colors
colors.dat <- data.frame(matrix(nrow = length(levels(dat.input1$item_color)),
                                ncol = 2))
colnames(colors.dat) <- c("item_color", "item.color.group")
colors.dat$item_color <- c(sort(levels(dat.input1$item_color)))
colors.dat$item_color <- as.factor(unlist(colors.dat$item_color))

# Group colors according to common color groups
color.matching <- c(
    "?"  =  "other"  ,          "almond" = "brown" ,       "amethyst" = "purple" , 
    "ancient" = "yellow",       "anthracite" = "black",    "antique pink" = "purple",   
    "apricot" = "yellow",       "aqua" = "blue" ,          "aquamarine" = "blue",    
    "ash" = "grey"   ,          "aubergine" = "purple"  ,  "aviator" = "other" ,     
    "avocado" = "green",        "azure" = "blue",          "baltic blue" = "blue",   
    "basalt" = "grey",          "beige" = "white",         "berry" = "red",          
    "black" = "black",          "blau" = "blue",           "blue" = "blue",          
    "bordeaux" = "red",         "brown" = "brown",         "brwon" = "brown",        
    "caramel" = "yellow",       "champagner" = "white",    "cobalt blue" = "blue",   
    "cognac" = "brown",         "copper coin" = "brown",   "coral" = "red",          
    "creme" = "yellow" ,        "crimson" = "red",         "curled" = "other",       
    "currant purple" = "red",   "curry" = "yellow",        "dark denim" = "blue",   
    "dark garnet" = "black",    "dark grey" = "grey",      "dark navy" = "blue",     
    "dark oliv" = "green",      "darkblue" = "blue" ,      "denim" = "blue" ,       
    "ebony" = "black" ,         "ecru" = "grey"    ,       "floral" = "other",       
    "fuchsia" = "red",          "gold" = "yellow",         "graphite" = "grey",      
    "green" = "green",          "grey" = "grey",           "habana" = "other",         
    "hibiscus" = "red",         "ingwer" = "brown",        "iron" = "grey",          
    "ivory" = "white",          "jade" = "green",          "kanel" = "brown",        
    "khaki" = "brown" ,         "lemon" = "yellow",        "magenta" = "red" ,       
    "mahagoni" = "brown",       "mango" = "yellow",        "mint" = "green",         
    "mocca" = "brown",          "nature" = "other",        "navy" = "blue",          
    "ocher" = "brown",          "oliv" = "green",          "olive" = "green",        
    "opal" = "other",           "orange" = "red",          "pallid" = "grey",        
    "perlmutt" = "other",       "petrol" = "blue",         "pink" = "purple",           
    "purple" = "purple",        "red" = "red",             "silver" = "grey",        
    "stained" = "other",        "striped" = "other",       "terracotta" = "red",   
    "turquoise" = "blue",       "vanille" = "yellow",       "white" = "white",        
    "yellow" = "yellow" )

colors.dat$item.color.group <- as.factor(color.matching)

dat.input1 <- merge(dat.input1, colors.dat, by = "item_color" )

# Some clean up
rm(colors.dat, color.matching)

### C) Item category and size ###

# Determine category of item based on (unique) size
sizes <- list()

# Clothing sizes
sizes$clothing.eu = c("xs", "XS", "s", "S", "m", "M", "l", "L", "xl", "XL", 
                      "xxl", "XXL", "xxxl", "XXXL","34", "36", "38", "40",
                      "42", "44", "46", "48", "50", "52", "54", "56" )
sizes$clothing.uk = c("6", "8", "10", "12", "14", "16", "18", "20", "22", "24")

# Shoe sizes (mostly women)
sizes$shoe.eu = c("35", "35+", "36", "36+",	"37", "37+", "38",	"38+",	
                  "39",	"39+", "40", "40+",	"41", "41+",
                  "42", "42+", "43", "43+", "44", "44+")
sizes$shoe.uk.us = c("3", "3+", "4", "4+", "5", "5+", "6", "6+", 
                     "7", "7+", "8", "8+", "9", "9+", "9+",
                     "10", "10+", "11", "11+")

# Children sizes
sizes$children.size = c("44",	"50",	"56",	"62",	"68",	"74",	"80",
                        "86",	"92",	"98",	"104",	"110",	"116",	"122",
                        "128","134",	"140",	"146",	"152",	"158",  "164",
                        "170",	"176")

sizes$pants.eu = c("3832", "3432", "3332", "3332", "23", "24", "25", "26",
                   "27", "28", "29", "30", "31", "32", "34", "36", "38",
                   "40", "42", "44", "48", "50", "52")

# Find unique sizes to determine category
k <- unlist(sizes)
z <- k[duplicated(k)]
sizes$unique <- k[!(k %in% z)]

vec.cloth <- unlist(sizes$unique[1:22])
vec.shoes <- unlist(sizes$unique[23:51])
vec.child <- unlist(sizes$unique[52:71])
vec.pants <- unlist(sizes$unique[72:82])

# Assign category to those items with unique size
dat.input1  <- dat.input1 %>% 
    group_by(item_id) %>% 
    dplyr:: mutate(
        item.category = ifelse(item_size %in% vec.cloth, "clothing","unknown"),
        item.category = ifelse(item_size %in% vec.shoes, "shoes", item.category),
        item.category = ifelse(item_size %in% vec.child, "child", item.category),
        item.category = ifelse(item_size %in% vec.pants, "pants", item.category))

# TODO: Refine size criteria (based on range + levels)

# Clean up

rm(sizes, k, z, vec.cloth, vec.child, vec.pants, vec.shoes)

### Define subcategories based on price  
subgroups  <- dat.input1 %>% 
    group_by(item.category) %>% 
    dplyr:: mutate(
        q1 = quantile(item_price, 0.25),
        q3 = quantile(item_price, 0.75))

dat.input1$item.subcategory <- ifelse(subgroups$item_price <= subgroups$q1,
                                      "cheap", "neutral")
dat.input1$item.subcategory <- ifelse(subgroups$item_price >= subgroups$q3,
                                      "luxus", dat.input1$item.subcategory)
rm(subgroups)

##### Note

## Order on day of registration
dat.input1$first.order <- as.factor(ifelse(dat.input1$user_reg_date == dat.input1$order_date, 1, 0))


