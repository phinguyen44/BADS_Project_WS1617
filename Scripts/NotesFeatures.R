############################################################################

### Feature extracion: Products

#  Calculate mean and count for brand return and sale
agg1 =  aggregate(dat.input$return, list(dat.input$item_id), 
                 FUN = function(x)c(mnreturn = mean(x), n = length(x)))

agg2 =  aggregate(dat.input$item_price, list(dat.input$item_id), 
                 FUN = function(x)c(mnprice = mean(x)))


agg3 = aggregate(dat.input$return, list(dat.input$item_id, dat.input$item_size), 
                       FUN = function(x)c(mnreturn = mean(x), n = length(x)))

agg4 = aggregate(agg3$Group.2, list(agg3$Group.1), 
                 FUN = function(x)c(numbersizes = length(x)))

agg5 = aggregate(dat.input$return, list(dat.input$item_id, dat.input$item_color), 
                 FUN = function(x)c(mnreturn = mean(x), n = length(x)))

agg6 = aggregate(agg5$Group.2, list(agg5$Group.1), 
                 FUN = function(x)c(numbercolours = length(x)))

product <- data.frame(cbind(do.call(data.frame, agg1), do.call(data.frame, agg2),
                            do.call(data.frame, agg4), do.call(data.frame, agg6)))
                      
colnames(product) = c("item_id", "aver.return.item", "sales.item",
                      "group", "mean.price.item", "group", "availablesizes",
                      "group", "availablecolurs")

product = product[, c(1,2,3,5,7,9)]

# Check distribution of sales and return

boxplot(product$sales.item, horizontal=TRUE,axes=TRUE,outline=FALSE)
summary(product$aver.return.item)

#### Cluster brands

### Use subset for clustering

# Use Euclidean distance
d <- dist(scale(product[,2:6]), method = "euclidean")

# hclust# Dendogramm suggests 3 cluster
cluster.hier <- hclust(d, method = "ward.D")
plot(cluster.hier)

clusters = cutree(cluster.hier, k=5) # get 5 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

# Get centroids
(centroids = sapply(unique(clusters), clust.centroid, scale(product[,2:6]), clusters))

# Calculate 3 cluster based on k-means
kcluster = kmeans(scale(product[,2:6]), centers = t(centroids))

memb = kcluster$cluster

table(memb, clusters)

# Check for suitability of cluster
sil <- silhouette(memb, d)
plot(sil, col=1:2, border=NA)

# Illustrate Cluster with PCA
PCA = prcomp(product[,2:6],center=F, scale=F)
summary(PCA)
PCA$rotation
pca = PCA$x[,1:3]
k = data.frame(cbind(pca, memb))

ggplot(k, aes(PC1, PC2, color = factor(memb)))+
    geom_point()

test = cbind(k, product)

dat.input = merge(dat.input, test[, c(1:5)], by = "item_id")
