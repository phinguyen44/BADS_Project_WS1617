################################################################################
# 
#   Feature Creation and Extraction
#
################################################################################
# Description:
# Create all supervised and unsupervised features to be built on training set
# Feature categories:
# A) item
# B) order
# C) customer
#
################################################################################

rm(list = ls())

# Adjust your working directory
setwd("")
getwd()

# LOAD NECESSARY PACKAGES & DATA
# List all packages needed for session
neededPackages <- c("cluster", "ggplot2", "tidyverse", "dplyr")
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
load("Data/BADS_WS1718_known_imp1.RData")

# source helper packages
source("Scripts/Helpful.R")

############################################################################
############################################################################
### A) Feature creation on item level ######################################

#############################################################################
###  Item price + discount ###

# Item price: Decretize price using equal binning
# @ Phi: please enter your code here, coudnt really do it
# make sure that the price of 0 is an extra category!


# Item discount and pric paid: calculate discount based on max price per item & size

dat.input1  <- dat.input1 %>% 
    group_by(item_id, item_size) %>% 
    dplyr::mutate(
        max.price.item.size    = max(item_price),
        min.price.item.size    = min(item_price),
        range.price.item.size  = max(item_price) - min(item_price),
        discount.abs           = max.price.item.size - item_price,
        discount.pc            = (max.price.item.size - item_price)/
                                 max.price.item.size,
        is.discount            = ifelse(discount.abs > 0, 1, 0)) %>% 
    dplyr::select(-max.price.item.size, -min.price.item.size, -range.price.item.size)

# Adjust discount in percentage for zero price
dat.input1$discount.pc  <- ifelse(dat.input1$discount.pc == "NaN",
                                  0, dat.input1$discount.pc )

# price bins (discrete)
price  <- num.check(dat.input1, "item_price")
priceB <- discrete.bin(price, numbins = 10)

# assign bins (manually add 0)
dat.input1$item_priceB  <- assign.bins(dat.input1, priceB, "item_price")
dat.input1$item_priceB  <- as.character(dat.input1$item_priceB)
idx <- which(dat.input1$item_price == 0)
dat.input1$item_priceB[idx] <- "[0,0]"
dat.input1$item_priceB  <- as.factor(dat.input1$item_priceB)

#############################################################################
### Item Colour ###

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

#############################################################################
### Item category and size ###

# Determine category of item based on (unique) size
sizes <- list()

# Clothing sizes
sizes$clothing.eu = c("xs", "XS", "s", "S", "m", "M", "l", "L", "xl", "XL", 
                      "xxl", "XXL", "xxxl", "XXXL", "32","34", "36", "38", "40",
                      "42", "44", "46", "48", "50", "52", "54", "56", "76", "80",
                      	"84" ,	"88" ,	"92", 	"96", 	"100", 	"105", 	"110", 	
                      "116", 	"122", 	"128",  "16" ,	"17", 	"18", 	"19", 
                      "20",     "21" ,	"22" ,	"23" ,	"24" ,	"25", 	"26", 	"27")
sizes$clothing.uk = c("4","6", "8", "10", "12", "14", "16", "18", "20", "22", "24")

# Shoe sizes (mostly women)
sizes$shoe.eu = c("32", "33", "34", "34+" ,"35", "35+", "36", "36+",	
                  "37", "37+", "38",   "38+", "39", "39+", "40", 
                   "40+",	"41", "41+", "42", "42+", "43", "43+",
                  "44", "44+", "45", "45+", "46", "46+", "47", "48", "49")
sizes$shoe.uk.us = c("3", "3+", "4", "4+", "5", "5+", "6", "6+", 
                     "7", "7+", "8", "8+", "9", "9+", "9+",
                     "10", "10+", "11", "11+", "12", "12+", "13", "13+", "14", "14+")

# Children sizes
sizes$children.size = c("44",	"50",	"56",	"62",	"68",	"74",	"80",
                        "86",	"92",	"98",	"104",	"110",	"116",	"122",
                        "128",  "134",	"140",	"146",	"152",	"158",  "164",
                        "170",	"176", "28", 	"30", 	"32", 	"34", 	"36", 
                        "38", 	"40" ,	"42",   "1" ,	"3" ,	"5", 	"7" ,
                        "9",    "11", 	"13", 	"15" ,	"17")

sizes$pants.eu = c("3832", "3432", "3332", "3332", "23", "24", "25", "26",
                   "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
                   "38", "37",  "40", "42", "44", "48", "50", "52", "54")

sizes$acessoires = c("unsized", "80", "90", "100", "110", "120", "130",
                     "140", "150", "160" )

# Find unique sizes to determine category
k <- unlist(sizes)
z <- k[duplicated(k)]
sizes$unique <- k[!(k %in% z)]

vec.cloth <- unlist(sizes$unique[1:21])
vec.shoes <- unlist(sizes$unique[22:51])
vec.child <- unlist(sizes$unique[52:66])
vec.pants <- unlist(sizes$unique[67:70])
vec.acces <- unlist(sizes$unique[71:76])

# Assign category to those items with unique size
dat.input1  <- dat.input1 %>% 
    group_by(item_id) %>% 
    dplyr:: mutate(
        item.categoryi = ifelse(item_size %in% vec.cloth, "clothing","unknown"),
        item.categoryi = ifelse(item_size %in% vec.shoes, "shoes", item.categoryi),
        item.categoryi = ifelse(item_size %in% vec.child, "child", item.categoryi),
        item.categoryi = ifelse(item_size %in% vec.pants, "pants", item.categoryi),
        item.categoryi = ifelse(item_size %in% vec.acces, "acces", item.categoryi),
        all.categories = list(item.categoryi),
        check.cat      = length(unique(unlist(all.categories))),
        item.category  = ifelse(check.cat > 2, "unknown", min(item.categoryi)))  %>% 
    dplyr:: select(
        -item.categoryi, - all.categories, -check.cat)

# Clean up
rm(sizes, k, z, vec.cloth, vec.child, vec.pants, vec.shoes, vec.acces)

### Define subcategories based on price  

dat.input1  <- dat.input1 %>% 
    group_by(item.category) %>% 
    dplyr::mutate(
        item.subcategory = ifelse(item_price <= quantile(item_price, 0.25),  "cheap", "neutral"),
        item.subcategory = ifelse(item_price >= quantile(item_price, 0.75), "luxus", item.subcategory))

### End of item based features  ###
#############################################################################
############################################################################
### B) Feature creation on order level ######################################
# Definition of basket: Items bought by customer on same day

# Create features based on ordered baskets
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_date) %>% 
    dplyr::mutate(
                    basket.value = sum(item_price),
                    basket.size  = n(),
                    basket.big   = ifelse(basket.size > 1, 1, 0))

# Find similar items within one basket (disregarding color/size)
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_date, item_id) %>% 
    dplyr::mutate(
        order.same.item    = n(),
        order.same.itemD   = ifelse(order.same.item > 1, 1, 0))

# Find similar items within one basket of different size
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_date, item_id) %>% 
    dplyr::mutate(
        item.basket.size.diff     = length(item_size),
        item.basket.size.diffD    = ifelse(item.basket.size.diff > 1, 1, 0))

# Find similar items within same category in one basket(regardless of size)
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_date, item.category) %>% 
    dplyr::mutate(
        item.basket.same.category   = n(),
        item.basket.same.categoryD  = ifelse(item.basket.same.category > 1, 1, 0))

# Find similar items within same category in one basket (differing in size)
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_date, item.category) %>% 
    dplyr::mutate(
        item.basket.category.size.diff   = length(item_size),
        item.basket.category.size.diffD  = ifelse(item.basket.category.size.diff > 1, 1, 0))

# First order on day of registration
dat.input1$first.order <- as.factor(ifelse(dat.input1$user_reg_date == 
                                           dat.input1$order_date, 1, 0))

### End of order based features  ###
############################################################################
### C) Feature creation on customer level ##################################

### Account age at time of order
dat.input1$account.age.order <- dat.input1$order_date - dat.input1$user_reg_date

### Value of total purchases user and number of purchases
dat.input1  <- dat.input1 %>% 
    group_by(user_id, order_item_id) %>% 
    dplyr::mutate(
        value.purchase.custom   = sum(item_price),
        items.purchase.custom   = n())

#### Construct income average for Bundeslaender
income.bl.dat <- data.frame(
    matrix(nrow = length(levels(dat.input1$user_state)), 
           ncol = 3))

# New data frame containing external information on Bundeslaender
colnames(income.bl.dat)  <- c("user_state", "income.bl", "WestGerm")
income.bl.dat$user_state <- as.factor(levels(dat.input1$user_state))
income.bl.dat$income.bl  <- c(42.62, 42.95, 35.42, 26.84, 46.75, 60.91,
                              42.73, 32.59, 25.02, 36.54, 33.58, 34.89,
                              27.89, 25.82, 30.48, 27.17)
income.bl.dat$WestGerm <- as.factor(c(1,1,0,0,1,1,1,1,0,1,1,1,0,0,1,0))

dat.input1 <- merge(dat.input1, income.bl.dat, by = "user_state" )

#### Construct income average for age groups

# New data frame containing external information on age
income.age.dat <- data.frame(
    matrix(nrow = length(levels(as.factor(dat.input1$age))), 
           ncol = 3))
colnames(income.age.dat)  <- c("age", "income.age", "age.group")
income.age.dat$age <- levels(as.factor(dat.input1$age))

# Assign income to groups and define age groups
income.age <- list()
age.group  <- list()
for (i in 1:length(levels(as.factor(dat.input1$age)))){
    if (i < 20){
        income.age[i] <- 8.51
        age.group[i] <-  "below20"
    } else if (i < 25){ 
        income.age[i] <- 17.90
        age.group[i]  <- "below25"
    } else if (i < 30){
        income.age[i] <- 26.79
        age.group[i]  <- "below30"
    } else if (i < 35){
        income.age[i] <- 33.56
        age.group[i]  <- "below35"
    } else if (i < 40){
        income.age[i] <- 36.30
        age.group[i]  <- "below40"
    } else if (i < 45){
        income.age[i] <- 38.44
        age.group[i] <- "below45"
    } else if (i < 50){
        income.age[i] <- 38.83
        age.group[i] <- "below50"
    } else if (i < 55){
        income.age[i] <- 38.74
        age.group[i]  <- "below55"
    } else if (i < 60){
        income.age[i] <- 36.82
        age.group[i] <- "below60"
    } else if (i < 65){
        income.age[i] <- 34.43
        age.group[i]  <- "below65"
    } else {
        income.age[i] <- 13.68
        age.group[i] <- "above65"
    }
}

income.age.dat$income.age <- as.numeric(income.age)
income.age.dat$age.group <- as.factor(unlist(age.group))

dat.input1 <- merge(dat.input1, income.age.dat, by = "age" )

# Calculate indicator of income based on age and state

# Geometric mean
dat.input1$income.ind      <- (dat.input1$income.bl * dat.input1$income.age)^0.5
dat.input1$price.inc.ratio <- dat.input1$item_price / dat.input1$income.ind

# Some clean up
rm(income.bl.dat, income.age.dat, income.age, age.group)

### End of customer based features  ###
#############################################################################
#############################################################################
### Addition to b) Grouping brands (must be done at the end) ################

# Group brands with clustering: Review whether this makes sense
brand.cluster  <- dat.input1 %>% 
    group_by(brand_id) %>% 
    dplyr::summarise(
        range.age.brand       = max(age) - min(age),
        mean.age.brand        = mean(age),
        mean.price.brand      = mean(item_price),
        range.price.brand     = max(item_price) - min(item_price),
        nu.orders.brand       = length(item_id),
        sum.price.brand       = sum(item_price),
        nu.items.brand        = length(unique(item_id)),
        nu.color.brand        = length(unique(item_color)),
        nu.size.brand         = length(unique(item_size)),
        nu.category.brand     = length(unique(item.category)),
        mean.income.brand     = mean(income.ind),
        range.income.brand    = max(income.ind) -min(income.ind))

# Dendogramm based on hclust suggests 3 clusters
d <- dist(scale(brand.cluster[,-1]), method = "euclidean")
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)

# Extract 3 clusters and centroids
clusters <- cutree(cluster.hier, k=3) 
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}
centroids <- sapply(unique(clusters), 
                   clust.centroid, scale(brand.cluster[,-1]), 
                   clusters)

# Calculate 3 cluster based on k-means
brand.cluster$brand.cluster <- kmeans(scale(brand.cluster[,-1]), 
                                centers = t(centroids))$cluster

dat.input1 <- merge(dat.input1, brand.cluster[, c(1,14)], by = "brand_id")

# Clean up
rm(brand.cluster, centroids)
#############################################################################
#############################################################################

# Final formatting
dat.input1 <- dat.input1 %>% 
    dplyr::mutate(item_id                  = factor(item_id),
           brand_id                        = factor(brand_id),
           user_id                         = factor(user_id),
           weekday                         = factor(weekday),
           account.age.order               = as.numeric(account.age.order),
           order_year                      = factor(order_year),
           item_id                         = factor(item_id),
           item_size                       = factor(item_size),
           item.category                   = factor(item.category),
           item.subcategory                = factor(item.subcategory),
           basket.big                      = factor(basket.big),
           order.same.itemD                = factor(order.same.itemD),
           item.basket.size.diffD          = factor(item.basket.size.diffD),
           item.basket.same.categoryD      = factor(item.basket.same.categoryD),
           item.basket.category.size.diffD = factor(item.basket.category.size.diffD),
           first.order                     = factor(first.order),
           brand.cluster                   = factor(brand.cluster)) %>% 
    dplyr::select(
                   -user_dob, -order_date, -order_item_id, -delivery_date,
                   -user_dob_year, -user_reg_date)

# Export clean data set
save(dat.input1, file = "BADS_WS1718_known_var.RData" )
