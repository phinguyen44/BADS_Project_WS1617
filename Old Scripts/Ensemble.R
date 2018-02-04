# ENSEMBLE

# initialize
acc.e    <- rep(NA, length(pred))
order.e  <- rep(NA, length(pred))
y.orig.e <- vector('list', length(pred))

# start with best classifier
class1        <- sapply(cMat, function(x) x$overall['Accuracy'])
names(class1) <- names(cMat)
acc.e[1]      <- class1[which(class1 == max(class1))]
order.e[1]    <- names(class1)[which(class1 == max(class1))]

y.idx  <- which(names(pred) == order.e[1])

y.orig.e <- pred[[y.idx]]


for (i in 2:length(class1)) {
    
    # NEED TO WEIGHT CORRECTLY
    
    # add classifiers and average predictions
    y.new <- lapply(pred, function(x) data.frame(cbind(y.orig.e, x)))
    y.avg <- lapply(y.new, rowMeans)
    
    # find best classifier score
    cMat.in <- lapply(y.avg, 
                      function(x) confusionMatrix(
                          round(x), ts.label, positive = "1"))
    class.in    <- sapply(cMat, function(x) x$overall['Accuracy'])
    names(class.in) <- names(cMat)
    
    idx.in      <- which(class.in == max(class.in))
    start.in    <- paste0(names(class.in)[idx.in])
    acc.in      <- class.in[idx.in]
    
    # stop if accuracy doesn't improve
    if (acc.in < acc.e[i-1]) {
        
        order.final <- order.e[i-1]
        acc.final   <- acc.e[i-1]
        
        break
    }
    
    # continue otherwise
    order.e[i] <- paste0(order.e[i-1], ',', start.in)
    acc.e[i]   <- acc.in
    
    # get new average
    y.orig.e <- y.new[[idx.in]]
    colnames(y.orig.e) <- unlist(strsplit(order.e[i], ','))
    
    order.final <- order.e[i]
    acc.final   <- acc.e[i]
    
}

order.final
acc.final

# AVERAGE RESULTS AS DESCRIBED
final.pred <- rowMeans(y.orig.e)

# apply threshold and get final costs
final.thresh <- find.threshold(ts.label, final.pred, ts.price)
final.pred.final <- ifelse(final.pred > final.thresh, 1, 0)
