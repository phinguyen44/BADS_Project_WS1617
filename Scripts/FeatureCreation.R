
################################################################################
# 
#   Feature Creation and Extraction
#
################################################################################
# Description:
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

### Brand-id: Use Weight of evidence

#  Calculate mean and count for brand return and sale
agg =  aggregate(dat.input$return, list(dat.input$brand_id), 
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
brand_agg$WOE.brand = log((brand_agg$bad/BAD)/(brand_agg$good/GOOD))

# Calculate Information Value

brand_agg$InVa = ((brand_agg$bad/BAD) - (brand_agg$good/GOOD)) * brand_agg$WOE.brand

# Overall IV = 0.15 medium predictor 
sum(brand_agg$InVa)


############################################################################
# Merge created variables

vec = c("brand_id", "aver.return.brand", "sales.brand", "WOE.brand")

dat.input = merge(dat.input, brand_agg[, vec], by = "brand_id" )

rm(BAD, GOOD, vec)

### Sort by order id again
dat.input = dat.input[order(dat.input$order_item_id),]

############################################################################

### Feature Extraction: brands

Mode <- function(x) {
    if (is.numeric(x)) {
        x_table <- table(x)
        return(as.numeric(names(x_table)[which.max(x_table)]))
    }
}

agg2 =  aggregate(dat.input$age, list(dat.input$brand_id), 
                  FUN = function(x)c(mn = mean(x),med = median(x),
                                     max = max(x), min = min(x)))

agg3 =  aggregate(dat.input$item_price, list(dat.input$brand_id), 
                  FUN = function(x)c(max = max(x),med = median(x), min = min(x), len = length(x)))

agg4 =  aggregate(dat.input$item_id, list(dat.input$brand_id), 
                  FUN = function(x)c(len = length(unique(x))))

agg5 =  aggregate(dat.input$order_month, list(dat.input$brand_id), 
                  FUN = function(x)c(mmon = Mode(as.numeric(x))))


agg6 =  aggregate(dat.input$item_color, list(dat.input$brand_id), 
                  FUN = function(x)c(len = length(unique(x))))


brand_agg2 <- cbind(do.call(data.frame, agg2), do.call(data.frame, agg3), 
                    do.call(data.frame, agg4), do.call(data.frame, agg5),
                    do.call(data.frame, agg6))

colnames(brand_agg2) = c("brand_id", "mean.age.brand", "median.age.brand",
                         "max.age.brand", "min.age.brand",
                         "group", "max.price.brand", "median.price.brand", 
                         "min.price.brand", "no.of.orders", "group","no.items.brand",
                         "group", "mode.month", "no.of.colours")

vec1 = c("brand_id", "aver.return.brand", "sales.brand", "WOE.brand")

vec2 = c("brand_id", "mean.age.brand", "max.age.brand", "min.age.brand", "max.price.brand",
         "median.price.brand", "min.price.brand", "no.of.orders", "no.items.brand",
         "mode.month", "no.of.colours")

clus.input = merge(brand_agg[, vec1], brand_agg2[, vec2], by = "brand_id" )

rm(agg2, agg3, brand_agg, vec1, vec2, brand_agg2, agg4, agg5)

clus.input$pdiff = clus.input$max.price.brand - clus.input$min.price.brand
clus.input$agediff = clus.input$max.age.brand - clus.input$min.age.brand


#### Cluster brands

### Use subset for clustering

# Define optimal clustering variable vector
vec.brand = c("aver.return.brand", "sales.brand", "WOE.brand", "mean.age.brand", "max.price.brand",
              "median.price.brand", "no.of.orders", "no.items.brand",
              "mode.month", "pdiff", "agediff")

# Scale data
clusdat <- cbind("brand_id" = clus.input[, c("brand_id")], 
                 scale(clus.input[, vec.brand]))



# Use Euclidean distance
d <- dist(clusdat[,2:12], method = "euclidean")

# hclust# Dendogramm suggests 3 cluster
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)

clusters = cutree(cluster.hier, k=3) # get 3 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

# Get centroids
(centroids = sapply(unique(clusters), clust.centroid, clusdat[,2:12], clusters))

# Calculate 3 cluster based on k-means
kcluster = kmeans(clusdat[,2:12], centers = t(centroids))

memb = kcluster$cluster

table(memb, clusters)

# Check for suitability of cluster
sil <- silhouette(memb, d)
plot(sil, col=1:2, border=NA)

# Illustrate Cluster with PCA
PCA = prcomp(clusdat[,2:12],center=F, scale=F)
summary(PCA)
PCA$rotation
pca = PCA$x[,1:3]
k = data.frame(cbind(pca, memb))

ggplot(k, aes(PC1, PC2, color = factor(memb)))+
    geom_point()


### Merge clusters with dataset

brand.cluster = cbind("brand_id" = clus.input[, "brand_id"], "brand.cluster" = memb)

dat.input = merge(dat.input, brand.cluster, by = "brand_id" )

dat.input = dat.input[order(dat.input$order_item_id),]

dat.input$brand.cluster = as.factor(dat.input$brand.cluster)

rm(list=(ls()[ls()!=c("dat.input")]))

############################################################################

### Feature Extraction: Item size

agg7 =  aggregate(dat.input$age, list(dat.input$item_size), 
                  FUN = function(x)c(max = max(x),mn = mean(x), min = min(x)))

agg8 =  aggregate(dat.input$return, list(dat.input$item_size), 
                  FUN = function(x)c(mn = mean(x), len = length(x)))

agg9 =  aggregate(dat.input$item_price, list(dat.input$item_size), 
                  FUN = function(x)c(mn = mean(x), max = max(x), min = min(x)))


item_agg <- cbind(do.call(data.frame, agg7), do.call(data.frame, agg8),
                  do.call(data.frame, agg9))

colnames(item_agg) = c("item_size", "max.age.size", "mean.age.size", "min.age.size",
                       "group", "mean.return.size", "no.orders.size",
                       "group", "mean.price.size", "max.price.size", "min.price.size")

item_agg$pdiff = item_agg$max.price.size - item_agg$min.price.size 

vec3 =  c("mean.age.size", "min.age.size", "max.age.size",
          "mean.return.size", "no.orders.size", "mean.price.size", 
          "pdiff")

#### Cluster sizes

### Use subset for clustering

# Scale data
clusdat = item_agg[, c("item_size", vec3)]
clusdat[, vec3] = scale(clusdat[, vec3])

# Use Euclidean distance

# Todo: check whether include size name for cluster
d <- dist(clusdat[,2:8], method = "euclidean")

# hclust# Dendogramm suggests more than 6 cluster,
# but would lead to too small groups
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)


clusters = cutree(cluster.hier, k=6) # get 6 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

# Get centroids
(centroids = sapply(unique(clusters), clust.centroid, clusdat[,2:8], clusters))

# Calculate 5 cluster based on k-means
kcluster = kmeans(clusdat[,2:8], centers = t(centroids))

memb = kcluster$cluster

table(memb, clusters)

# Check for suitability of cluster
sil <- silhouette(memb, d)
plot(sil, col=1:2, border=NA)


# Illustrate Cluster with PCA
PCA = prcomp(clusdat[,2:8],center=F, scale=F)
summary(PCA)
PCA$rotation
pca = PCA$x[,1:2]
k = data.frame(cbind(pca, memb))

ggplot(k, aes(PC1, PC2, color = factor(memb)))+
    geom_point()

clusdat$size.cluster = memb

### Merge item clusters with dataset

size.cluster = clusdat[, c("item_size", "size.cluster")] 

dat.input = merge(dat.input, size.cluster, by = "item_size" )

dat.input = dat.input[order(dat.input$order_item_id),]

dat.input$size.cluster = as.factor(dat.input$size.cluster)

rm(list=(ls()[ls()!=c("dat.input")]))


############################################################################
# Further notes
############################################################################

### WOE for sizes

#  Calculate mean and count for brand return and sale
agg6 =  aggregate(dat.input$return, list(dat.input$item_size), 
                  FUN = function(x)c(mn = mean(x), n = length(x), 
                                     good = mean(x)*length(x), bad = (1-mean(x))*length(x)))

size_agg <- do.call(data.frame, agg6)

colnames(size_agg) = c("item_size", "aver.return.size", "sales.size", "good", "bad")

rm(agg6)

# Check distribution of sales and return

boxplot(size_agg$sales.size, horizontal=TRUE,axes=TRUE,outline=FALSE)
summary(size_agg$sales.size)

# Function of Weight of Evidence

GOOD = sum(size_agg$good)
BAD  = sum(size_agg$bad)

size_agg$WOE = log((size_agg$bad/BAD)/(size_agg$good/GOOD))

### Improve measure according to Zdravevski (2010)

# Case 1: No cases in good and bad: Not existent

# Case 2: Either no cases in good or bad: WOE -> Inf
# Replace occurence (=0) with 1

size_agg$good = ifelse(size_agg$good == 0, 1, size_agg$good)

size_agg$bad = ifelse(size_agg$bad == 0, 1, size_agg$bad)    

# Redo Calculation of WOE
size_agg$WOE.size = log((size_agg$bad/BAD)/(size_agg$good/GOOD))

# Calculate Information Value

size_agg$InVa = ((size_agg$bad/BAD) - (size_agg$good/GOOD)) * size_agg$WOE

# Overall IV = 0.06 weak predictor 
sum(size_agg$InVa)


############################################################################

# Merge created variables

vec4 = c("item_size", "WOE.size")

dat.input = merge(dat.input, size_agg[, vec4], by = "item_size" )

rm(BAD, GOOD, vec4)

### Sort by order id again
dat.input = dat.input[order(dat.input$order_item_id),]


############################################################################

### Item-id: Use Weight of evidence

#  Calculate mean and count for brand return and sale
agg =  aggregate(dat.input$return, list(dat.input$item_id), 
                 FUN = function(x)c(mn = mean(x), n = length(x), 
                                    good = mean(x)*length(x), bad = (1-mean(x))*length(x)))

item_agg <- do.call(data.frame, agg)

colnames(item_agg) = c("item_id", "aver.return.item", "sales.item", "good", "bad")

rm(agg)

# Check distribution of sales and return

boxplot(item_agg$sales.item, horizontal=TRUE,axes=TRUE,outline=FALSE)
summary(item_agg$sales.item)

# Function of Weight of Evidence

GOOD = sum(item_agg$good)
BAD  = sum(item_agg$bad)

item_agg$WOE = log((item_agg$bad/BAD)/(item_agg$good/GOOD))

### Improve measure according to Zdravevski (2010)

# Case 1: No cases in good and bad: Not existent

# Case 2: Either no cases in good or bad: WOE -> Inf
# Replace occurence (=0) with 1

item_agg$good = ifelse(item_agg$good == 0, 1, item_agg$good)

item_agg$bad = ifelse(item_agg$bad == 0, 1, item_agg$bad)    

# Redo Calculation of WOE
item_agg$WOE.item = log((item_agg$bad/BAD)/(item_agg$good/GOOD))

# Calculate Information Value

item_agg$InVa = ((item_agg$bad/BAD) - (item_agg$good/GOOD)) * item_agg$WOE.item

# Overall IV = 0.45 very good predictor! 
sum(item_agg$InVa)


############################################################################


# Merge created variables

vec = c("item_id", "aver.return.item", "sales.item", "WOE.item")

dat.input = merge(dat.input, item_agg[, vec], by = "item_id" )

rm(BAD, GOOD, vec)

### Sort by order id again
dat.input = dat.input[order(dat.input$order_item_id),]


# Important note: For training purposes, WOE should only be calculated on training set!

hist(dat.input$WOE.item, nclass = 100)


############################################################################

### Feature creation based on basked


# Get all unique combinations of order date & user_id
baskets <- dat.input %>%
    group_by(user_id, order_date) %>% 
    summarize(basket.size=length(order_item_id)) %>% 
    as.data.frame(.) 


dat.input = merge(dat.input, baskets, by = c("user_id", "order_date") )

rm(baskets)

### Sort by order id again
dat.input = dat.input[order(dat.input$order_item_id),]



# Export clean data set

save(dat.input, file = "BADS_WS1718_known_var.RData" )


