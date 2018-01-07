
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
    dat.input <- load("BADS_WS1718_known_clean.RData")
    load("BADS_WS1718_known_clean.RData")

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

brand_agg$InVa = ((brand_agg$bad/BAD) - (brand_agg$good/GOOD)) * brand_agg$WOE

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


agg2 =  aggregate(dat.input$age, list(dat.input$brand_id), 
                 FUN = function(x)c(mn = mean(x),med = median(x)))

agg3 =  aggregate(dat.input$item_price, list(dat.input$brand_id), 
                  FUN = function(x)c(max = max(x),med = median(x)))

brand_agg2 <- cbind(do.call(data.frame, agg2), do.call(data.frame, agg3))

colnames(brand_agg2) = c("brand_id", "mean.age.brand", "median.age.brand",
                        "group", "max.price.brand", "median.price.brand")

vec1 = c("brand_id", "aver.return.brand", "sales.brand", "WOE.brand")

vec2 = c("brand_id", "median.age.brand", "max.price.brand",
        "median.price.brand")

clus.input = merge(brand_agg[, vec1], brand_agg2[, vec2], by = "brand_id" )

rm(agg2, agg3, brand_agg, vec1, vec2, brand_agg2)

#### Cluster brands

### Use subset for clustering

vec.brand = c("WOE.brand", "sales.brand",  "median.age.brand", "max.price.brand",
              "median.price.brand")

# Scale data
clusdat <- cbind("brand_id" = clus.input[, c("brand_id")], 
                            scale(clus.input[, vec.brand]))

# Use Euclidean distance
d <- dist(clusdat[,2:6], method = "euclidean")

# hclust# Dendogramm suggests 5 cluster
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)

clusters = cutree(cluster.hier, k=5) # get 5 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

# Get centroids
(centroids = sapply(unique(clusters), clust.centroid, clusdat[,2:6], clusters))

# Calculate 4 cluster based on k-means
kcluster = kmeans(clusdat[,2:6], centers = t(centroids))

memb = kcluster$cluster

table(memb, clusters)

# Check for suitability of cluster
sil <- silhouette(memb, d)
plot(sil, col=1:2, border=NA)

# Illustrate Cluster with PCA
PCA = prcomp(clusdat[,2:6],center=F, scale=F)
summary(PCA)
PCA$rotation
pca = PCA$x[,1:2]
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

# Export clean data set

save(dat.input, file = "BADS_WS1718_known_var.RData" )
    

       