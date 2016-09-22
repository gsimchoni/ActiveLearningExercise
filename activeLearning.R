getQueryIndices <- function(X, y, queryIdx = NULL, nClass = 10, totalSampleSize = 500, querySize = 1,
                            models, uncertainty, initSampleSize = 50, queryBagging = FALSE, nBag = 1) {
  
  # check that totalSampleSize isn't larger than total no. of unlabeled samples
  if (totalSampleSize > nrow(X)) {
    stop("totalSampleSize cannot be larger than total n")
  }
  
  # check that querySize isn't larger than totalSampleSize
  if (querySize > totalSampleSize) {
    stop("querySize cannot be larger than totalSampleSize")
  }
  
  # check that totalSampleSize - initSampleSize is a multiple of querySize
  if ((totalSampleSize - initSampleSize) %% querySize != 0) {
    stop("what's left of totalSampleSize after sampling initSampleSize is not a multiple of querySampleSize")
  }
  
  # How many iterations to run
  numIter <- totalSampleSize / querySize
  
  # If queryIdx is NULL, this means we're querying for the first time - need to use random sample
  if (is.null(queryIdx)) {
    queryIdx <- sample(nrow(X), initSampleSize)
    numIter <- numIter - (initSampleSize/querySize)
  }
  
  if (length(table(y[queryIdx])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  for (iter in 1:numIter) {
    cat("total no. of queries:", length(queryIdx), "\n")
    
    # Create data.frame to store predictions
    predDF <- data.frame(matrix(nrow = dim(X[-queryIdx,])[1], ncol = length(models) * nBag * nClass))
    # Get variations in columns, often = 0 when n is small and might be problematic
    sds <- apply(X[queryIdx,], 2, sd)
    tempTrainDF <- data.frame(y = y[queryIdx], X = X[queryIdx, which(sds > 0)])
    tempTestDF <- data.frame(y = y[-queryIdx], X = X[-queryIdx, which(sds > 0)])
    # Train models
    for (i in 1:length(models)) {
      if (!queryBagging) {
        predDF[, (nClass * (i - 1) + 1) : (nClass * i)] <- fitModel(models[i], tempTrainDF, tempTestDF)
      } else {
        for (b in 1:nBag) {
          queryIdxBagged <- sample(queryIdx, length(queryIdx), replace = TRUE)
          sds <- apply(X[queryIdxBagged,], 2, sd)
          tempTrainDF <- data.frame(y = y[queryIdxBagged], X = X[queryIdxBagged, which(sds > 0)])
          tempTestDF <- data.frame(y = y[-queryIdx], X = X[-queryIdx, which(sds > 0)])
          predDF[, (nClass * ((i - 1) * nBag + (b - 1)) + 1) : (nClass * ((i - 1) * nBag + b))] <- 
                   fitModel(models[i], tempTrainDF, tempTestDF)
        }
      }
    }
    
    # Choose next samples
    nextToQuery <- getNextQuery(predDF, querySize, uncertainty, length(models) * nBag, 1:nrow(X), queryIdx)
    
    # Add next samples to queryIdx
    queryIdx <- c(queryIdx, nextToQuery)
  }
  cat("total no. of queries:", length(queryIdx), "\n")
  
  # In case totalSampleSize %% querySize != 0 we have a little too many observations
  #queryIdx <- queryIdx[1:totalSampleSize] ###### THIS IS PROBLEMATIC ############
  
  # Return the query indices
  return(queryIdx)
}

checkInputValidity <- function(totalSampleSize, querySize, models, uncertainty, numReports, nData, pData,
                               pca, numPCs, initSampleSize, queryBagging, nBag) {
  # check that all models are in allowedModelsList
  if (any(!models %in% allowedModelsList)) {
    stop("You have specified a model not in the allowedModelsList")
  }
  
  # check that uncertainty is in the allowedUncertaintiesList
  if (!uncertainty %in% allowedUncertaintiesList) {
    stop("You have specified a uncertainty not in the allowedUncertaintiesList")
  }
  
  # check that totalSampleSize isn't larger than total no. of unlabeled samples
  if (totalSampleSize > nData) {
    stop("totalSampleSize cannot be larger than total n")
  }
  
  # check that querySize isn't larger than totalSampleSize
  if (querySize > totalSampleSize) {
    stop("querySize cannot be larger than totalSampleSize")
  }
  
  # check that totalSampleSize is a multiple of numReports
  if (totalSampleSize %% numReports != 0) {
    stop("totalSampleSize is not multiple of number of reports")
  }
  
  # check that querySize isn't larger than reportSampleSize (=totalSampleSize/numReports)
  if (querySize > (totalSampleSize / numReports)) {
    stop("querySize is larger than each report size")
  }
  
  # check that totalSampleSize - initSampleSize is a multiple of querySize
  if ((totalSampleSize - initSampleSize) %% querySize != 0) {
    stop("what's left of totalSampleSize after sampling initSampleSize is not a multiple of querySampleSize")
  }
  
  # check that uncertainty is allowed for no. of models
  if (uncertainty == "voteEntropy" & length(models) == 1 & (!queryBagging)) {
    stop("uncertainty voteEntropy does not make sense with a single model without baggnig")
  }
  if (uncertainty %in% c("leastConfident", "marginSampling", "entropy") & length(models) > 1) {
    stop("uncertainty chosen does not make sense with a committee of models")
  }
  
  # check that bagging parameters make sense
  if (!queryBagging & nBag > 1) {
    stop("if queryBagging = FALSE, nBag should be 1")
  }
  if (queryBagging & nBag == 1) {
    warning("bagging is usually made with nBag > 1")
  }
  
  # check PCA numPCs parameter if pca = TRUE
  if (pca) {
    # check that numPCs is specified
    if (is.na(numPCs)) {
      stop("You set pca = TRUE, Please specify Number of PCs")
    }
    # check that numPCs is lower than features dimension
    if (numPCs > pData) {
      stop("Number of PCs cannot be larger than no. of features")
    }
  }
}

getNextQuery <- function(pred, querySize, uncertainty, nModels, ids, queryIdx) {
  nextQueryIDs <- switch(uncertainty,
         leastConfident = leastConfident(pred, querySize),
         marginSampling = marginSampling(pred, querySize),
         entropy = entropy(pred, querySize),
         voteEntropy = voteEntropy(pred, querySize, nModels),
         voteMargin = voteMargin(pred, querySize, nModels))
  return(ids[-queryIdx][nextQueryIDs])
}

fitModel <- function(model, tempTrainDF, tempTestDF) {
  fit <- switch(model,
                gbm = gbm(y ~ ., data = tempTrainDF, distribution = "multinomial"),
                rf = randomForest(y ~ ., data = tempTrainDF, mtry = floor(sqrt(ncol(tempTrainDF)))),
                knn = knn3(y ~ ., data = tempTrainDF, k = 10),
                svm = svm(y ~ ., data = tempTrainDF, probability = TRUE),
                rpart = rpart(y ~ ., data = tempTrainDF),
                bag = bagging(y ~ ., data = tempTrainDF))
  pred <- switch(model,
                 gbm = predict(fit, tempTestDF, n.trees = 100, type = "response")[,,1],
                 rf = predict(fit, tempTestDF, type = "prob"),
                 knn = predict(fit, tempTestDF, type = "prob"),
                 svm = attributes(predict(fit, tempTestDF, probability = TRUE))$probabilities,
                 rpart = predict(fit, tempTestDF, type = "prob"),
                 bag = predict(fit, tempTestDF, type = "prob"))
  return(pred[, order(colnames(pred))])
}

runMNISTActiveLearning <- function(totalSampleSize = 500, querySize = 1, models, nClass = 10,
                                   uncertainty, pca = FALSE, numPCs = NA, numReports = 10,
                                   initSampleSize = 50, finalModel, queryBagging = FALSE, nBag = 1) {
 
  source("loadMnist.R")
  source("uncertainties.R")
  suppressMessages(library(caret))
  suppressMessages(library(gbm))
  suppressMessages(library(randomForest))
  suppressMessages(library(e1071))
  suppressMessages(library(rpart))
  suppressMessages(library(ipred))
  
  # Load MNIST datasets
  load_mnist()
  X <- trainSet$x/255
  y <- trainSet$y
  X.test <- testSet$x/255
  y.test <- testSet$y
  
  # Load allowedModelsList
  allowedModelsList <<- scan("resources/allowedModelsList.txt", what = "character", quiet = TRUE)
  
  # Load allowedUncertaintiesList
  allowedUncertaintiesList <<- scan("resources/allowedUncertaintiesList.txt", what = "character", quiet = TRUE)
  
  # Check input validity
  checkInputValidity(totalSampleSize, querySize, models, uncertainty, numReports, nrow(X), ncol(X),
                     pca, numPCs, initSampleSize, queryBagging, nBag)
  
  # Perform PCA if necessary
  if (pca) {
    # keep rotation
    rotation <- prcomp(X)$rotation[, 1:numPCs]
    X <- X %*% rotation
    X.test <- X.test %*% rotation
  }
  
  # Iterate between 50 and 500
  reportTotalSampleSize <- totalSampleSize / numReports
  
  # start with empty queryIdx
  queryIdx <- NULL
  
  # initialize accuracy vectors
  accuracyAL <- NULL
  accuracyPL <- NULL
  
  # y of MNIST trainSet needs to be factor
  y.tr.factor <- factor(y, labels = paste0("d", 0:(nClass - 1)))
  y.te.factor <- factor(y.test, labels = paste0("d", 0:(nClass - 1)))
  
  # create training data.frame convenient for gbm
  # teData <- data.frame(X = X.test, y = y.te.factor)
  
  # getting totalSampleSize random sample once
  randIdx <- sample(nrow(X), totalSampleSize)
  r <- randIdx[1:reportTotalSampleSize]
  if (length(table(y.tr.factor[r])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  queryIdx <- getQueryIndices(X, y.tr.factor, queryIdx, 10,
                              totalSampleSize, querySize, models, uncertainty, initSampleSize, queryBagging, nBag)
  q <- queryIdx[1:reportTotalSampleSize]
  if (length(table(y.te.factor[q])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  for (i in 1:numReports) {
    # Perform final model(s) on query
    sds <- apply(X[q, ], 2, sd)
    trData <- data.frame(X = X[q, which(sds > 0)], y = y.tr.factor[q])
    teData <- data.frame(X = X.test[, which(sds > 0)], y = y.te.factor)
    
    # Get AL test data performance
    allPredAL <- fitModel(finalModel, trData, teData)
    predAL <- apply(allPredAL, 1, function(x) which.max(x) - 1)
    accuracyAL[i] <- sum(predAL == y.test) / length(y.test)
    
    # Perform final model(s) on random totalSampleSize from train
    sds <- apply(X[r, ], 2, sd)
    randData <- data.frame(X = X[r, which(sds > 0)], y = y.tr.factor[r])
    teData <- data.frame(X = X.test[, which(sds > 0)], y = y.te.factor)
    
    # Get PL test data performance
    allPredPL <- fitModel(finalModel, randData, teData)
    predPL <- apply(allPredPL, 1, function(x) which.max(x) - 1)
    accuracyPL[i] <- sum(predPL == y.test) / length(y.test)
    
    # Get query indices
    q <- queryIdx[1:(reportTotalSampleSize * (i + 1))]
    r <- randIdx[1:(reportTotalSampleSize * (i + 1))]
  }
  
  confMatrixAL <- table(predAL, y.te.factor)
  confMatrixPL <- table(predPL, y.te.factor)
  
  # Save results
  
  # Graph results and save graphs
  plot(reportTotalSampleSize * (1:numReports), accuracyAL, type="l", col="red", xaxt="n", ylim = c(0, 1), xlab = "sample n")
  lines(reportTotalSampleSize * (1:numReports), accuracyPL, col="green")
  axis(1, reportTotalSampleSize * (1:numReports))
  
  return(list(accuracyAL = accuracyAL, accuracyPL = accuracyPL, confMatrixAL = confMatrixAL, confMatrixPL = confMatrixPL,
              randIdx = randIdx, queryIdx = queryIdx))
}