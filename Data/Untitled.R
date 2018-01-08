
################################################################################
# 
#   Feature Creation 2
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
neededPackages <- c("cluster", "ggplot2")
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
Train <- load("BADS_WS1718_known_clean.RData")
load("BADS_WS1718_known_clean.RData")

############################################################################

### Customer segments

#  Calculate mean and count for age return and sale
agg =  aggregate(Train$return, list(Train$size.cluster), 
                 FUN = function(x)c(mn = mean(x), n = length(x), 
                                    good = mean(x)*length(x), bad = (1-mean(x))*length(x)))

agg2 =  aggregate(Train$return, list(Train$user_id), 
                 FUN = function(x)c(mn = mean(x), n = length(x), 
                                    good = mean(x)*length(x), bad = (1-mean(x))*length(x)))

age_agg <- do.call(data.frame, agg)
    user_agg=    do.call(data.frame, agg2) 

colnames(age_agg) = c("age_id", "aver.return.age", "sales.age", "good", "bad")

rm(agg)


summary(user_agg$Group.1)

hist(user_agg$x.mn, nclass = 50)
hist(age_agg$x.mn, nclass = 80)


### item-id: Use Weight of evidence

#  Calculate mean and count for item return and sale
agg =  aggregate(Train$return, list(Train$item_id), 
                 FUN = function(x)c(mn = mean(x), n = length(x), 
                                    good = mean(x)*length(x), bad = (1-mean(x))*length(x)))

age_agg <- do.call(data.frame, agg)

colnames(age_agg) = c("item_id", "aver.return.item", "sales.item", "good", "bad")

rm(agg)

# Check distribution of sales and return

boxplot(age_agg$aver.return.item, horizontal=TRUE,axes=TRUE, outline=FALSE)
summary(age_agg$item_id)

# Function of Weight of Evidence

GOOD = sum(age_agg$good)
BAD  = sum(age_agg$bad)

age_agg$WOE.item = log((age_agg$bad/BAD)/(age_agg$good/GOOD))

### Improve measure according to Zdravevski (2010)

# Case 1: No cases in good and bad: Not existent

# Case 2: Either no cases in good or bad: WOE -> Inf
# Replace occurence (=0) with 1

age_agg$good = ifelse(age_agg$good == 0, 1, age_agg$good)

age_agg$bad = ifelse(age_agg$bad == 0, 1, age_agg$bad)    

# Redo Calculation of WOE
age_agg$WOE.item = log((age_agg$bad/BAD)/(age_agg$good/GOOD))

# Calculate Information Value

age_agg$InVa = ((age_agg$bad/BAD) - (age_agg$good/GOOD)) * age_agg$WOE.item

# Overall IV = 0.41 good predictor 
sum(age_agg$InVa)

hist(age_agg$aver.return.item, nclass=1000)


agg2 =  aggregate(Train$age, list(Train$item_id), 
                  FUN = function(x)c(mn = mean(x),med = median(x)))

agg3 =  aggregate(Train$item_price, list(Train$item_id), 
                  FUN = function(x)c(max = max(x),med = median(x), min = min(x), len = length(x)))


item_agg2 <- cbind(do.call(data.frame, agg2), do.call(data.frame, agg3))


colnames(item_agg2) = c("item_id", "mean.age.item", "median.age.item",
                         "group", "max.price.item", "median.price.item", 
                         "min.price.item", "no.of.orders")

item_agg2$diff = item_agg2$max.price.item - item_agg2$min.price.item

    

clus.input = merge(item_agg2, age_agg, by = "item_id" )
item = c("WOE.item", "diff", "no.of.orders")


clusdat <- cbind("item_id" = clus.input[, c("item_id")], 
                 scale(clus.input[, item]))

# Use Euclidean distance
d <- dist(clusdat[,2:4], method = "euclidean")

# hclust# Dendogramm suggests 5 cluster
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)

clusters = cutree(cluster.hier, k=3) # get 5 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

# Get centroids
(centroids = sapply(unique(clusters), clust.centroid, clusdat[,2:4], clusters))

# Calculate 5 cluster based on k-means
kcluster = kmeans(clusdat[,2:4], centers = t(centroids))

memb = kcluster$cluster

table(memb, clusters)

# Check for suitability of cluster
sil <- silhouette(memb, d)
plot(sil, col=1:2, border=NA)

# Illustrate Cluster with PCA
PCA = prcomp(clusdat[,2:4],center=F, scale=F)
summary(PCA)
PCA$rotation
pca = PCA$x[,1:3]
k = data.frame(cbind(pca, memb))

ggplot(k, aes(PC2, PC1, color = factor(memb)))+
    geom_point()

clus.input$size.cluster = memb


size.cluster = clus.input[, c("item_id", "size.cluster", "WOE.item")] 

Train = merge(Train, size.cluster, by = "item_id" )

Train = Train[order(Train$order_item_id),]

Train$size.cluster.y = as.factor(Train$size.cluster)



