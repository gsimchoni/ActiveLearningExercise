### This is a script to run experiments in deck ###

# Import necessary scripts --------------------------------------------------------------------------------
source("MNISTwrapper.R")
source("experiments_plotting.R")

# Experiment 1: one model, varying uncertainty ------------------------------------------------------------

uncertainties <- c("leastConfident", "marginSampling", "entropy")

res1 <- list()

for (i in 1:length(uncertainties)) {
  res1[[i]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = "rf", nClass = 10,
                                     uncertainty = uncertainties[i], numReports = 10, initSampleSize = 50, finalModel = "rf")
}

# get the data
al <- sapply(res1, function(x) x$accuracyAL)
pl <- res1[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp1.png')
plot.res(data, c(0.5, 1), "Experiment 1: one model, varying uncertainty", 
         c("random", "leastConfident", "marginSampling", "entropy"),
         "uncertainty")
dev.off()


# Experiment 2: one model, varying querySize ------------------------------------------------------------

querySizes <- c(1, 5, 10, 25, 50)

res2 <- list()

for (i in 1:length(querySizes)) {
  res2[[i]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = querySizes[i], models = "rf", nClass = 10,
                                     uncertainty = "marginSampling", numReports = 10, initSampleSize = 50, finalModel = "rf")
}

# get the data
al <- sapply(res2, function(x) x$accuracyAL)
pl <- res2[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp2.png')
plot.res(data, c(0.5, 1), "Experiment 2: one model, varying querySize", 
         c("random", querySizes),
         "querySize")
dev.off()

# Experiment 3: one model, varying type ------------------------------------------------------------

models <- c("gbm","rf","bag","svm","rpart","knn")

res3 <- list()

for (i in 1:length(models)) {
  res3[[i]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = models[i], nClass = 10,
                                      uncertainty = "marginSampling", numReports = 10, initSampleSize = 50, finalModel = models[i])
}

for (i in 1:length(models)) {
  # get the data
  al <- sapply(res3[i], function(x) x$accuracyAL)
  pl <- res3[[i]]$accuracyPL
  n <- 50 * (1:10)
  data <- data.frame(n = n, pl = pl, al = al)
  
  # plot and save to PNG file
  png(paste0('exp_graphs/exp3_', models[i], '.png'))
  plot.res(data, c(0.2, 1), paste0("Experiment 3: one model: ", models[i]),
           c("random", "AL"),
           models[i])
  dev.off()
}

# Experiment 4: committee of models, entropy ------------------------------------------------------------

models <- c("gbm","rf","bag","svm","rpart","knn")
# remove knn/svm if they're too slow
models <- c("gbm","rf","bag","svm","rpart")

res4 <- list()

for (i in 2:length(models)) {
  res4[[i-1]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = models[1:i], nClass = 10,
                                      uncertainty = "voteEntropy", numReports = 10, initSampleSize = 50, finalModel = "rf")
}

# get the data
al <- sapply(res4, function(x) x$accuracyAL)
pl <- res4[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp4.png')
idx <-list(1:2,1:3,1:4,1:5)
committees <-sapply(lapply(idx, function(x) models[x]), paste, collapse = ", ")
plot.res(data, c(0.5, 1), "Experiment 4: committee of models, entropy", 
         c("random", committees),
         "commmittees")
dev.off()


# Experiment 5: committee of models, margin ------------------------------------------------------------

models <- c("gbm","rf","bag","svm","rpart","knn")
# remove knn/svm if they're too slow, or leave to the end
models <- c("gbm","rf","bag","svm","rpart")

res5 <- list()

for (i in 2:length(models)) {
  res5[[i-1]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = models[1:i], nClass = 10,
                                      uncertainty = "voteMargin", numReports = 10, initSampleSize = 50, finalModel = "rf")
}

# get the data
al <- sapply(res5, function(x) x$accuracyAL)
pl <- res5[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp5.png')
idx <-list(1:2,1:3,1:4,1:5)
committees <-sapply(lapply(idx, function(x) models[x]), paste, collapse = ", ")
plot.res(data, c(0.5, 1), "Experiment 5: committee of models, margin", 
         c("random", committees),
         "commmittees")
dev.off()


# Experiment 6: one model, query bagging ------------------------------------------------------------

nBags <- c(3, 5, 10)

res6 <- list()

for (i in 1:length(nBags)) {
  res6[[i]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = "rf", nClass = 10,
                                      uncertainty = "voteEntropy", numReports = 10, initSampleSize = 100, finalModel = "rf",
                                      queryBagging = TRUE, nBag = nBags[i])
}

# get the data
al <- sapply(res6, function(x) x$accuracyAL)
pl <- res6[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp6.png')
plot.res(data, c(0.5, 1), "Experiment 6: one model, query bagging", 
         c("random", nBags),
         "nBag")
dev.off()

# Experiment 7: committee of models + bagging ------------------------------------------------------------

nBags <- c(3, 5)
models <- c("gbm","rf")

res7 <- list()

for (i in 1:length(nBags)) {
  res7[[i]] <- runMNISTActiveLearning(totalSampleSize = 500, querySize = 10, models = models, nClass = 10,
                                        uncertainty = "voteEntropy", numReports = 10, initSampleSize = 100, finalModel = "rf",
                                        queryBagging = TRUE, nBag = nBags[i])
}

# get the data
al <- sapply(res7, function(x) x$accuracyAL)
pl <- res7[[1]]$accuracyPL
n <- 50 * (1:10)
data <- data.frame(n = n, pl = pl, al = al)

# plot and save to PNG file
png('exp_graphs/exp7.png')
plot.res(data, c(0.5, 1), "Experiment 7: committee of models + bagging", 
         c("random", nBags),
         "(gbm, rf) x")
dev.off()


# Save results
save(res1, res2, res3, res4, res5, res6, res7, file = "results.RData")
