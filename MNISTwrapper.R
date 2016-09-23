#' This function wraps activeLearning main function getQueryIndices, specifically with
#' the MNIST dataset. You can specify many query strategies combinations, you will get the
#' final query indices, the accuracy on the MNIST test set (x num interim Reports, e.g. from 50 to 500),
#' and comparison to regular random sampling with the same no. of interim reports
#' 
#' @param totalSampleSize The total no. of samples to query, a.k.a the Budget
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param models A character vector of model(s) to use (see allowedModelsList)
#' @param nClass Number of classes
#' @param uncertainty Type of uncertainty (see allowedUncertaintiesList)
#' @param pca Logical indicating whether to perform PCA on MNIST, speeds up things
#' @param numPCs The number of principal components to keep from PCA
#' @param numReports The number of interim steps/reports to make and record performance
#' @param initSampleSize The no. of initial random samples to start with
#' @param finalModel The model used to predict the class on the final query data
#' @param queryBagging Logical indicating Whether to perform qury_bagging to create more committee members
#' @param nBag Number of bagged samples for each model in models
#' @param verbose Logical indicating whther to print messages
#' @return List of confusion matrices for AL (Active Learning) and PL (Passive Learning), vectors of
#' AL and PL reported test accuracies, the totalSampleSize query indices and random indices used for comparison

runMNISTActiveLearning <- function(totalSampleSize = 500, querySize = 1, models, nClass = 10,
                                   uncertainty, pca = FALSE, numPCs = NA, numReports = 10,
                                   initSampleSize = 50, finalModel, queryBagging = FALSE, nBag = 1, verbose = TRUE) {
  message("\nStarting AL process on MNIST dataset")
  source("activeLearning.R")
  source("loadMnist.R")
  source("utils.R")
  
  # Load MNIST datasets
  message("Loading MNIST dataset")
  load_mnist()
  X <- trainSet$x/255
  y <- trainSet$y
  X.test <- testSet$x/255
  y.test <- testSet$y
  
  message("Checking input validity")
  
  # Load allowedModelsList
  allowedModelsList <<- scan("resources/allowedModelsList.txt", what = "character", quiet = TRUE)
  
  # Check input validity
  checkInputValidityMNISTWrapper(totalSampleSize, numReports, pca, numPCs, ncol(X), finalModel)
  
  # Load finalModel relevant package after it was validated
  loadPackage(finalModel)
  
  # Perform PCA if necessary project on desired no. of first PCs
  if (pca) {
    message("Performing PCA on X, could take a while")
    # keep rotation
    rotation <- prcomp(X)$rotation[, 1:numPCs]
    X <- X %*% rotation
    X.test <- X.test %*% rotation
  }
  
  # Size of interim step to record and report performance
  reportTotalSampleSize <- totalSampleSize / numReports
  
  # Start with empty queryIdx
  queryIdx <- NULL
  
  # Initialize accuracy vectors
  accuracyAL <- NULL
  accuracyPL <- NULL
  
  # y of MNIST trainSet needs to be factor
  y.tr.factor <- factor(y, labels = paste0("d", 0:(nClass - 1)))
  y.te.factor <- factor(y.test, labels = paste0("d", 0:(nClass - 1)))
  
  # Getting totalSampleSize random sample once
  randIdx <- sample(nrow(X), totalSampleSize)
  # Get the first reportSampleSize random indices, make sure they include all nClass labels
  r <- randIdx[1:reportTotalSampleSize]
  if (length(table(y.tr.factor[r])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  # Call AL main function to get the totalSampleSize samples indices to query
  message("\nCalling Active Learning to get query indices")
  queryIdx <- getQueryIndices(X, y.tr.factor, queryIdx, 10, totalSampleSize, querySize,
                              models, uncertainty, initSampleSize, queryBagging, nBag, verbose)
  
  # Get the first reportSampleSize query indices, make sure they include all nClass labels
  q <- queryIdx[1:reportTotalSampleSize]
  if (length(table(y.te.factor[q])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  # Start adding more and mmore sammples and record performance of final model
  message("\nStarting recording performance of final model with more and more samples")
  for (i in 1:numReports) {
    if (verbose) message("Running report ", i)
    
    # Get AL test data performance
    # Getting rid of zero variance features, creating temp train/test datasets
    sds <- apply(X[q, ], 2, sd)
    trData <- data.frame(X = X[q, which(sds > 0)], y = y.tr.factor[q])
    teData <- data.frame(X = X.test[, which(sds > 0)], y = y.te.factor)
    allPredAL <- fitModel(finalModel, trData, teData)
    predAL <- apply(allPredAL, 1, function(x) which.max(x) - 1)
    accuracyAL[i] <- sum(predAL == y.test) / length(y.test)
    
    # Get PL (random) test data performance
    sds <- apply(X[r, ], 2, sd)
    randData <- data.frame(X = X[r, which(sds > 0)], y = y.tr.factor[r])
    teData <- data.frame(X = X.test[, which(sds > 0)], y = y.te.factor)
    allPredPL <- fitModel(finalModel, randData, teData)
    predPL <- apply(allPredPL, 1, function(x) which.max(x) - 1)
    accuracyPL[i] <- sum(predPL == y.test) / length(y.test)
    
    # Add next reportSampleSize indices from queryIdx and from randIdx
    q <- queryIdx[1:(reportTotalSampleSize * (i + 1))]
    r <- randIdx[1:(reportTotalSampleSize * (i + 1))]
  }
  message("Finished recording performance\n")
  
  confMatrixAL <- table(predAL, y.te.factor)
  confMatrixPL <- table(predPL, y.te.factor)
  
  # Graph results
  plot(reportTotalSampleSize * (1:numReports), accuracyAL, type="l", col="red",
       xaxt = "n", ylim = c(0, 1), xlab = "Sample N", ylab = "Accuracy",
       main = "Accuracy vs. Sample Size, AL vs. PL")
  lines(reportTotalSampleSize * (1:numReports), accuracyPL, col="green")
  axis(1, reportTotalSampleSize * (1:numReports))
  legend("bottomright", col = c("red", "green"), legend = c("AL", "PL"), lwd = 1)
  
  # Return list of results
  return(list(accuracyAL = accuracyAL, accuracyPL = accuracyPL, confMatrixAL = confMatrixAL, confMatrixPL = confMatrixPL,
              randIdx = randIdx, queryIdx = queryIdx))
}

#' Function for MNIST-AL basic input validation
#' 
#' @param totalSampleSize The total no. of samples to query, a.k.a the Budget
#' @param numReports The number of interim steps/reports to make and record performance
#' @param pca Logical indicating whether to perform PCA on MNIST, speeds up things
#' @param numPCs The number of principal components to keep from PCA
#' @param pData Number of features in X
#' @param finalModel The model used to predict the class on the final query data
checkInputValidityMNISTWrapper <- function(totalSampleSize, numReports, pca, numPCs, pData, finalModel) {
  # check that totalSampleSize is a multiple of numReports
  # TODO: get rid of this, we can add an extra report of needed
  if (totalSampleSize %% numReports != 0) {
    stop("totalSampleSize is not multiple of number of reports")
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
  
  # check that all models are in allowedModelsList
  if (!finalModel %in% allowedModelsList) {
    stop("You have specified a final model not in the allowedModelsList")
  }
}