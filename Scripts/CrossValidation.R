################################################################################
# CrossValidation.R
#
################################################################################
# Description:
#
# BADS project - build candidate models, do cross-validation, build ensemble
#
################################################################################

################################################################################
# LOAD DATA
rm(list = ls())

# Adjust your working directory
wd = file.path(Sys.getenv("HOME"),"documents/projects/bads-ws1718-group21")
setwd(wd)

# List all packages needed for session
neededPackages <- c("tidyverse", "magrittr", "purrr", "infuser",
                    "caret", "mlr",
                    "xgboost", "gbm", "rpart", "e1071", "MASS", "nnet",
                    "pROC", "parallel", "parallelMap")
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

# Load data
load("Data/BADS_WS1718_known_ready.RData")
df.known   <- read.csv("Data/BADS_WS1718_known.csv")

# Source performance metric calculations
source("Scripts/Helpful.R")
source("Scripts/Helpful-Models.R")

################################################################################
# INITIAL SETUP

# reorder and convert to numeric variables
df.train <- dat.ready %>%
    dplyr::mutate(return = as.integer(levels(return))[return]) %>% 
    dplyr::select(
        # DEMOGRAPHIC VARS
        age, 
        account.age.order,
        user_id, # WOE
        user.total.items, user.total.expen,
        # BASKET VARS
        deliver.time, 
        basket.big, basket.size, 
        item.basket.size.same, item.basket.size.diff, item.basket.same.category,
        no.return,
        # ITEM VARS
        item_id, item_size, brand_id, # WOE
        discount.pc, 
        item_price, 
        return)

# SAVE DATA FOR LATER
df.label <- df.train$return
df.price <- df.train$item_price

## LIST OF FUNCTIONS
learners <- list(lr = "classif.logreg",
                 rf = "classif.randomForest",
                 nn  = "classif.nnet",
                 xgb = "classif.xgboost")
mods <- list(lr = lr.mod,
             rf = rf.mod,
             nn  = nn.mod,
             xgb = xgb.mod)

################################################################################
# CROSS-VALIDATION

# split training set into k-folds
# inner loop calculates WOE for each split, does hyperparameter tuning
# outer loop estimates out-of-sample performance
# final model is made by fitting model (including hyperparameter tuning) to whole data set

set.seed(321)
k     <- 5
folds <- createFolds(df.train$return, k = k, list = TRUE)
str(folds)

foldruntime <- rep(0, k)
yhat        <- vector("list", length = k)
yhat.r      <- yhat
actual      <- yhat
ts.price.f  <- yhat

for (i in 1:k) {

    print(infuse("Starting Run #: {{iter}}", iter = i))

    start <- Sys.time()

    tr.f <- df.train[-folds[[i]], ] # in CV training
    ts.f <- df.train[folds[[i]], ]  # in CV test

    tr.label.f <- tr.f$return
    ts.label.f <- ts.f$return

    ts.price.f[[i]] <- ts.f$item_price
    
    # standardize
    tr.f <- z.scale(tr.f)
    ts.f <- z.scale(ts.f)

    # add in WOE variables
    tr.f$user_id_WOE <- WOE(tr.f, "user_id")
    tr.f$item_id_WOE <- WOE(tr.f, "item_id")
    tr.f$item_size_WOE <- WOE(tr.f, "item_size")
    tr.f$brand_id_WOE <- WOE(tr.f, "brand_id")

    user_id_WOE <- tr.f %>%
        dplyr::select(user_id, user_id_WOE) %>% distinct
    item_id_WOE <- tr.f %>%
        dplyr::select(item_id, item_id_WOE) %>% distinct
    item_size_WOE <- tr.f %>%
        dplyr::select(item_size, item_size_WOE) %>% distinct
    brand_id_WOE <- tr.f %>%
        dplyr::select(brand_id, brand_id_WOE) %>% distinct

    # apply WOE labels to test set
    ts.f <- ts.f %>%
        left_join(user_id_WOE, "user_id") %>%
        left_join(item_id_WOE, "item_id") %>%
        left_join(item_size_WOE, "item_size") %>%
        left_join(brand_id_WOE, "brand_id")

    # 0 out NA's
    ts.f[is.na(ts.f)] <- 0

    # select right variables for dataset
    tr.f <- tr.f %>%
        dplyr::select(
            # DEMOGRAPHIC VARS
            age, 
            account.age.order,
            user_id_WOE, # WOE
            user.total.items, user.total.expen,
            # BASKET VARS
            deliver.time, 
            basket.big, basket.size, 
            item.basket.size.same, item.basket.size.diff, 
            item.basket.same.category,
            no.return,
            # ITEM VARS
            item_id_WOE, item_size_WOE, brand_id_WOE, # WOE
            discount.pc, 
            item_price, 
            return)

    ts.f <- ts.f %>%
        dplyr::select(
            # DEMOGRAPHIC VARS
            age, 
            account.age.order,
            user_id_WOE, # WOE
            user.total.items, user.total.expen,
            # BASKET VARS
            deliver.time, 
            basket.big, basket.size, 
            item.basket.size.same, item.basket.size.diff, 
            item.basket.same.category,
            no.return,
            # ITEM VARS
            item_id_WOE, item_size_WOE, brand_id_WOE, # WOE
            discount.pc, 
            item_price, 
            return)

    # TRAIN MODEL
    yhat[[i]]   <- map2(mods, learners,
                        function(f, x) f(x, tr.f, ts.f, calib=TRUE))

    # GET ACTUAL VALUES AND STORE THEM
    actual[[i]] <- ts.label.f

    end <- Sys.time()
    foldruntime[i] <- end - start

}

################################################################################
# CROSS-VALIDATION RESULTS

# Check stability of cross-validation (metaparameters, costs)
alldata  <- transpose(yhat)
alldata2 <- lapply(alldata, transpose)

# predictions for each model
preds     <- lapply(alldata2,
                    function(x) lapply(x$pred, function(y) y$data$prob.1))
preds.r   <- lapply(preds,
                    function(x) lapply(x, function(y) round(y)))
p.calib   <- lapply(alldata2,
                    function(x) lapply(x$pred.calib, function(y) y$data$prob.1))
p.calib.r <- lapply(p.calib,
                    function(x) lapply(x, function(y) round(y)))

# Find appropriate threshold
thresh.list       <- preds
thresh.list.calib <- preds

for (i in 1:length(learners)) { # 4
    d       <- data.frame()
    d.calib <- data.frame()

    for (j in 1:k) {  # 5

        print(paste0("Get Threshold: ", i, ",", j))

        initial <- find.threshold(act  = actual[[j]],
                                  pred = preds[[i]][[j]],
                                  cost = ts.price.f[[j]])
        init.ca <- find.threshold(act  = actual[[j]],
                                  pred = p.calib[[i]][[j]],
                                  cost = ts.price.f[[j]])
        d       <- rbind(d, initial)
        d.calib <- rbind(d.calib, init.ca)

        # TODO: get new prediction list after setting threshold
        # should be five

    }
    thresh.list[[i]]       <- d
    thresh.list.calib[[i]] <- d.calib
}

# Get predictions after setting threshold
preds.thresh   <- preds
p.calib.thresh <- preds
run.cost       <- preds
run.cost.c     <- preds

for (i in 1:length(learners)) {
    for (j in 1:k) {
        val <- ifelse(preds[[i]][[j]] <= thresh.list[[i]][j, ]$threshold, 0, 1)
        preds.thresh[[i]][[j]] <- val
        run.cost[[i]][[j]]     <- thresh.list[[i]][j, ]$cost

        val2 <- ifelse(p.calib[[i]][[j]] <=
                           thresh.list.calib[[i]][j, ]$threshold, 0, 1)
        p.calib.thresh[[i]][[j]] <- val2
        run.cost.c[[i]][[j]]     <- thresh.list.calib[[i]][j, ]$cost

    }
}
preds.thresh.t   <- transpose(preds.thresh)
p.calib.thresh.t <- transpose(p.calib.thresh)
run.cost.t       <- transpose(run.cost)
run.cost.c.t     <- transpose(run.cost.c)
run.cost.avg     <- lapply(run.cost.t, function(x) mean(unlist(x)))
run.cost.c.avg   <- lapply(run.cost.c.t, function(x) mean(unlist(x)))
run.cost.c.avg # avg. run cost for each iteration

# Use this mean and save
thresh.mean.l       <- lapply(thresh.list, function(x) mean(x$threshold))
thresh.mean.l.calib <- lapply(thresh.list.calib, function(x) mean(x$threshold))
save(thresh.mean.l, file = 'Data/CalibratedThreshold.Rdata')

# Get avg. cost and standard error
avg.cost   <- lapply(thresh.list, function(x) mean(x$cost))
avg.cost.c <- lapply(thresh.list.calib, function(x) mean(x$cost))

se.cost    <- lapply(thresh.list, function(x) sd(x$cost)/sqrt(k))
se.cost.c  <- lapply(thresh.list.calib, function(x) sd(x$cost)/sqrt(k))

avg.cost.c
se.cost.c

# hyperparameters (examine)
hp    <- lapply(alldata2[2:4], function(x) lapply(x$pars, function(y) y))
hp.t  <- lapply(hp, transpose)
hp.df <- lapply(hp.t, function(x)
    data.frame(matrix(unlist(x), ncol = length(x))))
colnames(hp.df$nn)  <- names(hp.t$nn)
colnames(hp.df$xgb) <- names(hp.t$xgb)
colnames(hp.df$rf)  <- names(hp.t$rf)

hp.df$nn
hp.df$xgb
hp.df$rf

# prediction accuracy (just for good measure)
get.acc    <- function(x, act) confusionMatrix(x, act, positive="1")$overall[1]
pred.acc   <- lapply(preds.r, function(x) map2_dbl(x, actual, get.acc))
pred.acc.c <- lapply(p.calib.r, function(x) map2_dbl(x, actual, get.acc))

avg.acc   <- sapply(pred.acc, mean)
avg.acc.c <- sapply(pred.acc.c, mean)

acc.se   <- lapply(pred.acc, function(x) sd(x)/length(x))
acc.c.se <- lapply(pred.acc.c, function(x) sd(x)/length(x))

################################################################################
# BENCHMARK EXPERIMENTS

# reorganize predictions for simplicity (used for benchmark experiment)
pf.up   <- transpose(preds)
pf.c.up <- transpose(p.calib)
pf1     <- pf.up[[1]]
pf1c    <- pf.c.up[[1]]
act1    <- actual[[1]]
cost1   <- ts.price.f[[1]]

####### RESULTS FROM CALIBRATION:

# Reliability plots to compare original vs calibrated results (for 1 fold)
for (i in 1:length(mods)) {
    pdf(infuse("Written/Images/ReliabilityPlot-{{model}}.pdf",
               model = names(mods[i])),
        width  = 6,
        height = 4)
    reliability.plot(act1, pf1[[i]], pf1c[[i]], 10)
    dev.off()
}
dev.new()

# show average logLoss improvements from calibration
ll <- data.frame(
    sapply(preds, function(x) map2_dbl(actual, x, ModelMetrics::logLoss))
)
ll.c <- data.frame(
    sapply(p.calib, function(x) map2_dbl(actual, x, ModelMetrics::logLoss))
)
ll.avg   <- apply(ll, 2, mean)
ll.c.avg <- apply(ll.c, 2, mean)

ll.compare <- data.frame(cbind(ll.avg, ll.c.avg))
colnames(ll.compare) <- c('Uncalibrated', 'Calibrated')
ll.compare 
# improves log loss for all except log reg (which is already well-calibrated)

# show avg. cost / accuracy changes due to calibration
cost.compare <- data.frame(cbind(unlist(avg.cost), unlist(avg.cost.c)))
colnames(cost.compare) <- c('Uncalibrated', 'Calibrated')
cost.compare

acc.compare <- data.frame(cbind(unlist(avg.acc), unlist(avg.acc.c)))
colnames(acc.compare) <- c('Uncalibrated', 'Calibrated')
acc.compare

####### RESULTS FROM CROSS-VALIDATION (USING CALIBRATED RESULTS):

# plot cost bands
cost.df <- data.frame(cbind(unlist(avg.cost.c), unlist(se.cost.c)))
colnames(cost.df) <- c('Cost', 'SE')
cost.df$Model <- rownames(cost.df)

p <- ggplot(data = cost.df, aes(x = Model, y = Cost)) +
    geom_point() +
    geom_errorbar(aes(ymin = Cost - SE, ymax = Cost + SE)) +
    labs(title = "Total Cost") +
    labs(subtitle = infuse("{{num}}-Fold Cross-Validation", num=k)) +
    theme(plot.title = element_text(size=16)) +
    theme_bw()
p
ggsave('Written/Images/CV-cost.pdf', width = 6, height = 4)

# plot accuracy bands
acc.df <- data.frame(cbind(unlist(avg.acc.c), unlist(acc.c.se)))
colnames(acc.df) <- c('Acc', 'SE')
acc.df$Model <- rownames(acc.df)

p2 <- ggplot(data = acc.df, aes(x = Model, y = Acc)) +
    geom_point() +
    geom_errorbar(aes(ymin = Acc - SE, ymax = Acc + SE)) +
    labs(title = "Accuracy") +
    labs(subtitle = infuse("{{num}}-Fold Cross-Validation", num=k)) +
    theme(plot.title = element_text(size=16)) +
    theme_bw()
p2
ggsave('Written/Images/CV-accuracy.pdf', width = 6, height = 4)

####### RESULTS FROM THRESHOLD OPTIMIZATION (USING CALIBRATED RESULTS):

# plot threshold / cost plots for each model (for 1 fold)
for (i in 1:length(pf1c)) {
    g <- plot.threshold(act1, pf1c[[i]], cost1)
    ggsave(infuse('Written/Images/Threshold-{{mod}}.pdf', mod = names(pf1c[i])),
           plot   = g,
           height = 4,
           width  = 8)
}

####### RESULTS FROM ENSEMBLING (USING CALIBRATED RESULTS):

fin.ens  <- map2(p.calib.thresh.t, run.cost.c.t, function(x, y) ensembler(x, y))
fin.cost <- pmap(list(actual, fin.ens, ts.price.f),
                 function(x, y, z) cost.calc(0.5, x, y, z))
ens.df <- data.frame(cbind(unlist(run.cost.c.avg), unlist(fin.cost)))
colnames(ens.df) <- c('Average.Individual.Classifier', 'Ensembled.Classifier')
ens.df$Percent.Improvement <-  -round((ens.df[, 2]-ens.df[, 1])/ens.df[, 1], 4)
ens.df

# SAVE ALL ENVIRONMENT VARIABLES
save.image('Data/CV-results.RData')