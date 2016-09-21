getQueryIndices <- function(X, y, queryIdx = NULL, nClass = 10, totalSampleSize = 500, querySize = 1,
                            models, uncertainty) {
  
  # check that totalSampleSize isn't larger than total no. of unlabeled samples
  if (totalSampleSize > dim(X)[1]) {
    stop("totalSampleSize cannot be larger than total n")
  }
  
  # check that querySize isn't larger than totalSampleSize
  if (querySize > totalSampleSize) {
    stop("querySize cannot be larger than totalSampleSize")
  }
  
  # check that totalSampleSize is a multiple of querySize
  if (totalSampleSize %% querySize != 0) {
    stop("totalSampleSize is not a multiple of querySize (will change in the future")
  }
  
  # How many iterations to run
  numIter <- totalSampleSize / querySize
  
  # If queryIdx is NULL, this means we're querying for the first time - need to use random sample
  if (is.null(queryIdx)) {
    queryIdx <- sample(dim(X)[1], querySize)
    numIter <- numIter - 1
  }
  
  # If y isn't a factor try to make one...
  if (!is.factor(y)) {
    labels = paste0("l", 0:(length(table(y)) - 1))
    y <- factor(y, labels = labels)
  }
  
  ctrl <- trainControl(number = 1,
                       classProbs = TRUE,
                       verboseIter = FALSE,
                       savePredictions = FALSE)
  
  for (iter in 1:numIter) {
    # Create data.frame to store predictions
    predDF <- data.frame(matrix(nrow = dim(X[-queryIdx,])[1], ncol = length(models) * nClass))
    # Train models
    for (i in 1:length(models)) {
      fit <- train(x = X[queryIdx, ], y = y[queryIdx],
                   method = models[i],
                   trControl=ctrl,
                   verbose=FALSE)
      predDF[, (nClass * (i - 1) + 1) : (nClass * i)] <- predict(fit, X[-queryIdx,], type = "prob")
    }
    
    # Choose next samples
    nextToQuery <- getNextQuery(predDF, querySize, uncertainty, length(models), 1:dim(X)[1], queryIdx)
    
    # Add next samples to queryIdx
    queryIdx <- c(queryIdx, nextToQuery)
  }
  
  # In case totalSampleSize %% querySize != 0 we have a little too many observations
  #queryIdx <- queryIdx[1:totalSampleSize] ###### THIS IS PROBLEMATIC ############
  
  # Return the query indices
  return(queryIdx)
}

checkInputValidity <- function(totalSampleSize, querySize, models, uncertainty, numReports, nData, pData, pca, numPCs) {
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
  
  # check that each report size is a multiple of querySize
  if ((totalSampleSize / numReports) %% querySize != 0) {
    stop("each report size is not a multiple of querySize (will change in the future")
  }
  
  # check that uncertainty is allowed for no. of models
  if (uncertainty == "voteEntropy" & length(models) == 1) {
    stop("uncertainty voteEntropy does not make sense with a single model")
  }
  if (uncertainty %in% c("leastConfident", "marginSampling", "entropy") & length(models) > 1) {
    stop("uncertainty chosen does not make sense with a committee of models")
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
  nextQueryIDs = switch(uncertainty,
         leastConfident = leastConfident(pred, querySize),
         marginSampling = marginSampling(pred, querySize),
         entropy = entropy(pred, querySize),
         voteEntropy = voteEntropy(pred, querySize, nModels))
  return(ids[-queryIdx][nextQueryIDs])
}

runMNISTActiveLearning <- function(totalSampleSize = 500, querySize = 1, models, nClass = 10,
                                   uncertainty, pca = FALSE, numPCs = NA, numReports = 10) {
  # Load MNIST datasets
  load_mnist()
  X <- trainSet$x
  y <- trainSet$y
  X.test <- testSet$x
  y.test <- testSet$y
  
  # Load allowedModelsList
  allowedModelsList <<- scan("resources/allowedModelsList.txt", what = "character", quiet = TRUE)
  
  # Load allowedUncertaintiesList
  allowedUncertaintiesList <<- scan("resources/allowedUncertaintiesList.txt", what = "character", quiet = TRUE)
  
  # Check input validity
  checkInputValidity(totalSampleSize, querySize, models, uncertainty, numReports, dim(X)[1], dim(X)[2], pca, numPCs)
  
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
  teData <- data.frame(X = X.test, y = y.te.factor)
  colnames(teData) = c(colnames(X), "y")
  
  for (i in 1:numReports) {
    # Get query indices
    queryIdx <- getQueryIndices(X, y.tr.factor, queryIdx, 10,
                                reportTotalSampleSize, querySize, models, uncertainty)
    
    # Perform final model(s) on query
    trData <- data.frame(X = X[queryIdx, ], y = y.tr.factor[queryIdx])
    colnames(trData) = c(colnames(X), "y")
    
    fitAL <- gbm(y ~ ., data = trData, n.trees = 100, distribution = "multinomial")
    
    # Get AL test data performance
    allPredAL <- predict(fitAL, teData, n.trees = 100, type = "response")[,,1]
    predAL <- apply(allPredAL, 1, function(x) which.max(x) - 1)
    accuracyAL[i] <- sum(predAL == y.test) / length(y.test)
    
    # Perform final model(s) on random totalSampleSize from train
    randSample <- sample(dim(X)[1], i * reportTotalSampleSize)
    randData <- data.frame(X = X[randSample, ], y = y.tr.factor[randSample])
    colnames(randData) = c(colnames(X), "y")
    
    fitPL <- gbm(y ~ ., data = randData, n.trees = 100, distribution = "multinomial")
    
    # Get PL test data performance
    allPredPL <- predict(fitPL, teData, n.trees = 100, type = "response")[,,1]
    predPL <- apply(allPredPL, 1, function(x) which.max(x) - 1)
    accuracyPL[i] <- sum(predPL == y.test) / length(y.test)
  }
  
  
  # Save results
  
  # Graph results and save graphs
  plot(reportTotalSampleSize * (1:numReports), accuracyAL, type="l", col="red", xaxt="n")
  lines(reportTotalSampleSize * (1:numReports), accuracyPL, col="green")
  
  return(list(accuracyAL = accuracyAL, accuracyPL = accuracyPL))
}