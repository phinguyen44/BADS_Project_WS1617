################################################################################
# Helpful.R
#
################################################################################
# Description:
# Helpful functions designed to aid in BADS project
#
# DATA EXPLORATION:
# dist.check() - compares distribution of cat variables in training & test sets
# return.check() - looks at return rates by category in training set
# num.check() - looks at return rates for a numerical variable in training set 
# discrete.bins() - organizes observations into selected # of bins
# discrete.power() - organizes observations into bins (bins organized in size 
# by power)
# assign.bins() - creates bins based off bins created in discrete.bins()
# WOE() - self-made WOE function. outputs a vector
# 
# MODEL BUILDING:
# reliability.plot() - diagram to assess how 'calibrated' a classifier is
# 
# MODEL EVALUATION:
# acc.calc() - calculates accuracy for a given threshold
# cost.calc() - calculates costs (from cost matrix) for a given threshold
# find.threshold() - finds optimal threshold that minimizes costs
# plot.threshold() - plots cost and accuracy at different thresholds
# 
################################################################################

################################################################################
# DATA EXPLORATION

# check distribution of categorical variables (and how they might differ)
dist.check <- function(df.train, var) {
    
    dist.train <- data.frame(
        train = table(df.train[[var]])[order(table(df.train[[var]]))]
        )
    dist.test <- data.frame(
        test = table(df.test[[var]])[order(table(df.test[[var]]))]
        )
  
    dister   <- dist.train %>% 
        full_join(dist.test, by = c("train.Var1" = "test.Var1"))
  
    dister[is.na(dister)]    <- 0
    dister$Difference        <- 2 * dister[, 3] / dister[, 2] # should be near 1
  
    names(dister$train.Var1) <- "Variable"
    return(dister)
}

# check return rates of cat variables
return.check <- function(df, var) {
    return.table <- as.data.frame(as.matrix.data.frame(
        table(df[[var]], df$return)
        ))
    return.table[[var]] <- levels(df[[var]])
    return.table <- return.table %>% 
        dplyr::select(var, V1, V2) %>% 
        rename(Keep = V1, Return = V2) %>% 
        mutate(Total = Keep + Return, ReturnRate = round(Return / Total, 3))
  
    return(return.table)
}

# check return rates of numeric variables
num.check <- function(df, var) {
    prices.df <- df %>% arrange_(var)
    tables    <- as.data.frame(table(prices.df[[var]], prices.df$return))
    tables    <- tidyr::spread(tables, Var2, Freq)
    tables    <- tables %>% 
        rename(Keep = '0', Return = '1') %>% 
        mutate(Total      = Keep + Return, 
               ReturnRate = round(Return / Total, 3),
               Var1       = as.numeric(levels(Var1))[Var1])
  
  return(tables)
}

# Function creates discrete buckets based on chosen variable
# Note that fxn will try to create numbins selected, but if some observations 
# span multiple bins, there may be fewer bins
discrete.bin <- function(df, numbins = 10) {
    df <- df %>% 
        arrange(Var1) %>% 
        mutate(allsums = cumsum(Total))
    
    cutoff  <- round(tail(df, 1)$allsums / numbins)
    binsmax <- as.integer(seq(cutoff, tail(df, 1)$allsums, by = cutoff))
    if (length(binsmax) < numbins) {binsmax <- c(binsmax, tail(df, 1)$allsums)}
    
    # last value underbins
    binidx  <- sapply(binsmax, function(x) last(which(df$allsums <= x))) 
    
    maxval <- df$Var1[binidx]
    
    # Create 0 only bin
    maxval <- c(0, 0, maxval)
    
    df$bins  <- paste0("[0, ", maxval[2], "]")
    
    for (i in 2:length(maxval)) {
        for (j in 1:nrow(df)) {
            if (df$Var1[j] > maxval[i]) {
                df$bins[j] <- paste0("(", maxval[i], ", ", maxval[i+1], "]")
            }
        } 
    }
    
    df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
        group_by(bins) %>% 
        dplyr::summarize(ReturnRate = sum(Return) / sum(Total))
    
    return(df.bins)
}

# create a discrete function that follows power
discrete.power <- function(df, numbins = 10, powerval = 2) {
    df <- df %>% 
    arrange(Var1)
    
    cutoffs <- powerval ^ (1:numbins)
    if (max(cutoffs) > max(df$Var1)) {
        message("Bin values exceeds num items in group. Truncating # of bins.")
        cutoffs <- cutoffs[cutoffs < max(df$Var1)] 
    }
    # Create 0 only bin
    groupings    <- c(0, 0, cutoffs, max(df$Var1))

    df$bins  <- paste0("[0, ", groupings[2], "]")
    
    for (i in 2:length(groupings)) {
        for (j in 1:nrow(df)) {
            if (df$Var1[j] > groupings[i]) {
                df$bins[j] <- paste0("(", groupings[i], ", ", 
                                     groupings[i+1], "]")
            }
        }
    }
    
    df.bins <- df %>% 
    mutate(bins = factor(bins, levels = unique(bins))) %>% 
        group_by(bins) %>% 
        dplyr::summarize(ReturnRate = sum(Return) / sum(Total))
    
    return(df.bins)
}

# Create buckets in dataset
assign.bins <- function(df, buckets, variable) {
    start    <- unlist(gregexpr(buckets$bins, pattern = ", "))
    end      <- unlist(gregexpr(buckets$bins, pattern = "]"))
    ceilings <- c(as.numeric(substr(buckets$bins, start + 2, end - 1)))
    
    # set arbitrarily large ceiling
    ceilings[length(ceilings)] <- 99999
    
    grouping <- cut(df[[variable]], 
                    ceilings, 
                    include.lowest = FALSE)
    return(grouping)
}

# WOE
WOE <- function(df, var) {
    
    require(dplyr)
    require(magrittr)
    
    df.new <- df %>% 
        group_by_(var) %>% 
        dplyr::summarize(Keep   = n() - sum(return),
                  Return = sum(return))
    
    ### Improve measure according to Zdravevski (2010)
    tot.keep <- sum(df.new$Keep)
    tot.ret  <- sum(df.new$Return)
    
    # Case 1: Keep = 0, Return = 0 -> WOE = 0
    # Case 2: Keep = 0, Return > 0 -> Keep = Keep + 1, 
    # Return = Return + tot.ret/tot.keep
    # Case 3: Keep > 0, Return = 0 -> Return = Return + 1, 
    # Keep = Keep + tot.keep/tot.ret
    # Otherwise, normal case.
    df.new$WOE <- with(df.new, 
        ifelse(Keep == 0 & Return == 0, 0,
        ifelse(Keep == 0 & Return > 0, log((Return*tot.keep + tot.ret)/tot.ret),
        ifelse(Keep > 0 & Return == 0, log(tot.keep/(Keep*tot.ret + tot.keep)),
        log((Return/tot.ret)/(Keep/tot.keep))))))
    
    # join data
    out <- left_join(df, df.new, var) %>% use_series(WOE)
    return(out)
    
}

################################################################################
# MODEL BUILDING

reliability.plot <- function(act, pred, pred.c, bins = 10) {
    # act: vector of actual values. 0 or 1
    # pred: vector of predictions. real number between 0 and 1
    # bins: number of bins to use
    if(!require("Hmisc")) install.packages("Hmisc")
    library(Hmisc)
    
    bin.pred   <- cut(pred, bins)
    bin.pred.c <- cut(pred.c, bins)
    df         <- data.frame(act    = act, 
                             pred   = pred, 
                             pred.c = pred.c, 
                             bin    = bin.pred,
                             bin.c  = bin.pred.c)
    grouped  <- df %>% 
        dplyr::group_by(bin) %>% 
        dplyr::summarize(x   = sum(act) / n(), 
                         y   = mean(pred))
    
    grouped.c <- df %>% 
        dplyr::group_by(bin.c) %>% 
        dplyr::summarize(x   = sum(act) / n(), 
                         y   = mean(pred.c))
    
    plot(grouped$y, grouped$x, 
         xlim = c(0,1), ylim = c(0,1), 
         xlab = "Mean Prediction", 
         ylab = "Observed Fraction", 
         col  = "red", type = "o", main = "Reliability Plot")
    lines(c(0,1), c(0,1), col = "grey")
    lines(grouped.c$y, grouped.c$x, 
          xlim = c(0,1), ylim = c(0,1), 
          col  = "blue", type = "o")
    legend("topleft",
           lty    = c(1,1),lwd = c(2.5,2.5),
           col    = c("blue", "red"),
           legend = c("calibrated", "without calibration"),
           bty    = "n",
           cex    = 0.6)
    subplot(hist(pred, xlab = "", ylab = "", main = "", xlim = c(0,1),
                 col="red"),
            grconvertX(c(0.8, 1), "npc"), grconvertY(c(0.08, .25), "npc"))
    subplot(hist(pred.c, xlab = "", ylab = "", main = "", xlim = c(0,1),
                 col="blue"),
            grconvertX(c(0.6, 0.8), "npc"), grconvertY(c(0.08, .25), "npc"))
    
}

################################################################################
# MODEL EVALUATION

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
    best.cutoff <- combined$threshold[which.max(all.cost)]
    min.cost    <- max(all.cost)
    
    final <- data.frame(threshold = best.cutoff, cost = min.cost)
    
    return(final)
}

# plot 
plot.threshold <- function(act, pred, cost) {
    
    threshold <- seq(0, 1, by = 0.01)
    
    all.acc  <- sapply(threshold, function(x) acc.calc(x, act, pred))
    all.cost <- sapply(threshold, 
                       function(x) cost.calc(x, act, pred, cost))
    combined <- data.frame(threshold = threshold, 
                           accuracy  = all.acc, 
                           cost      = all.cost)
    best.cutoff <- combined$threshold[which.max(all.cost)]
    
    require(gridExtra)
    # plot
    p1 <- ggplot(data = combined, aes(x = threshold, y = accuracy)) +
        geom_line() + 
        geom_vline(xintercept = best.cutoff, color = "red", alpha = 0.7) +
        geom_label(aes(best.cutoff, min(accuracy), label = best.cutoff), 
                   size = 3, color = "red") + 
        
        labs(title = "Accuracy") + 
        labs(subtitle = "Red line threshold minimizes costs") + 
        
        theme(plot.title = element_text(size=16)) +
        theme(plot.subtitle = element_text(size=8)) + 
        theme_bw()
    
    p2 <- ggplot(data = combined, aes(x = threshold, y = cost)) +
        geom_line() + 
        geom_vline(xintercept = best.cutoff, color = "red", alpha = 0.7) +
        geom_label(aes(best.cutoff, min(all.cost), label = best.cutoff), 
                   size = 3, color = "red") + 
        
        labs(title = "Total Cost") + 
        labs(subtitle = "Red line threshold minimizes costs") + 
        
        theme(plot.title = element_text(size=8)) +
        theme(plot.subtitle = element_text(size=10)) + 
        theme_bw()
    
    return(grid.arrange(p1, p2, ncol=2))
}
