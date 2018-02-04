# TODO: do mean classification and mean costs

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"/Documents/Projects/bads-ws1718-group21")
setwd(wd)

load("Data/Predictions - Phi/run_20180123_y.Rdata")
load("Data/Predictions - Phi/run_20180123_yhat.Rdata")

library(tidyverse)
library(caret)

# sample costs
sample.cost   <- sample(seq(10, 400, by = 10), 20000, replace = TRUE)

################################################################################
# APPROACH # 1 - FIND OPTIMAL THRESHOLD

# accuracy
acc.calc <- function(threshold, act, pred) {
    decision  <- ifelse(pred > threshold, 1, 0)
    confusion <- table(Prediction = decision, True = act)
    accuracy  <- sum(diag(confusion)) / sum(confusion)
    return(accuracy)
}

# costs
cost.calc <- function(threshold, act, pred, cost) {
    compare <- data.frame(act = act, pred = pred, cost = cost)
    compare <- compare %>% 
        dplyr::mutate(decision = case_when(pred > threshold  ~ 1,
                                           pred <= threshold ~ 0),
                      class    = case_when(act == decision & act == 1 ~ "TP",
                                           act == decision & act == 0 ~ "TN",
                                           act != decision & act == 1 ~ "FN",
                                           act != decision & act == 0 ~ "FP"),
                      penalty  = case_when(class == "FP" ~ -0.5*cost,
                                           class == "FN" ~ -0.5*5*(3+0.1*cost),
                                           class %in% c("TP", "TN") ~ 0))
    
    sum.costs <- sum(compare$penalty)
    return(sum.costs)
}

# find threshold for given predictions
find.threshold <- function(act, pred, cost) {
    
    threshold <- seq(0, 1, by = 0.01)
    
    all.acc  <- sapply(threshold, function(x) acc.calc(x, act, pred))
    all.cost <- sapply(threshold, 
                       function(x) cost.calc(x, act, pred, cost))
    combined <- data.frame(threshold = threshold, 
                           accuracy  = all.acc, 
                           cost      = all.cost)
    best.cutoff <- combined$threshold[which(all.cost == max(all.cost))]
    min.cost    <- max(all.cost)
    
    final <- data.frame(threshold = best.cutoff, cost = min.cost)
    
    return(final)
}

# plot 
plot.threshold <- function(act, pred, cost) {
    
    threshold <- seq(0, 1, by = 0.02)
    
    all.acc  <- sapply(threshold, function(x) acc.calc(x, act, pred))
    all.cost <- sapply(threshold, 
                       function(x) cost.calc(x, act, pred, cost))
    combined <- data.frame(threshold = threshold, 
                           accuracy  = all.acc, 
                           cost      = all.cost)
    best.cutoff <- combined$threshold[which(all.cost == max(all.cost))]
    
    require(gridExtra)
    # plot
    p1 <- ggplot(data = combined, aes(x = threshold, y = accuracy)) +
        geom_line() + 
        geom_vline(xintercept = best.cutoff, color = "red", alpha = 0.7) +
        geom_label(aes(best.cutoff, min(accuracy), label = best.cutoff), 
                   size = 3, color = "red") + 
        
        labs(title = "Accuracy") + 
        labs(subtitle = "Red line indicates cutoff that minimizes costs") + 
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    p2 <- ggplot(data = combined, aes(x = threshold, y = cost)) +
        geom_line() + 
        geom_vline(xintercept = best.cutoff, color = "red", alpha = 0.7) +
        geom_label(aes(best.cutoff, min(all.cost), label = best.cutoff), 
                   size = 3, color = "red") + 
        
        labs(title = "Total Cost") + 
        labs(subtitle = "Red line indicates cutoff that minimizes costs") + 
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=10, color = "#7F7F7F"))
    
    return(grid.arrange(p1, p2, ncol=2))
}

lapply(pred, function(x) find.threshold(ts.label, x, sample.cost))
plots <- sapply(pred, function(x) plot.threshold(ts.label, x, sample.cost))

################################################################################
# APPROACH # 2 - BAYES NAIVE CLASSIFIER

# cotss are positive so we will try to minimize
c.fp <- 0.5 * sample.cost           # rev lost from FP misclass
c.fn <- 0.5 * (3 + 0.1*sample.cost) # rev lost from FN misclass

# creating the risk vector as described in Bahnsen 2015
# we don't have costs associated with TP or TN, so:
# R(Ci = 0 | Xi) = C.FNi*Pi
# R(Ci = 1 | Xi) = C.FPi*(1-Pi)
# where Pi is example i's predicted probability of return
# Ci is binary variable of classifying example i as return (1) or not (0)
# If R(Ci = 0 | Xi) < R(Ci = 1 | Xi), then we classify as 0

# risk for classifying as keep
risk.c0 <- data.frame(sapply(pred, function(x) round(c.fn * x, 2)))

# risk for classifying as return
risk.c1 <- data.frame(sapply(pred, function(x) round(c.fp * (1 - x), 2)))

decision <- risk.c0
for (i in 1:ncol(risk.c0)) {
    decision[, i] <- ifelse(risk.c0[, i] < risk.c1[, i], 0, 1)
}

# calculate costs
lapply(decision, function(x) cost.calc(threshold = 0.5, pred = x, act = ts.label, cost = sample.cost))
