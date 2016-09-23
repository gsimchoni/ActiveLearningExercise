#' The main Active Learning function which return which samples are best to query
#' 
#' @param X The unlabeled dataset
#' @param y The labels *factor* vector
#' @param queryIdx The indices of samples already queried (default is NULL)
#' @param nClass Number of classes
#' @param totalSampleSize The total no. of samples to query, a.k.a the Budget
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param models A character vector of model(s) to use (see allowedModelsList)
#' @param uncertainty Type of uncertainty (see allowedUncertaintiesList)
#' @param initSampleSize The no. of initial random samples to start with
#' @param queryBagging Logical indicating Whether to perform qury_bagging to create more committee members
#' @param nBag Number of bagged samples for each model in models
#' @param verbose Logical indicating whther to print messages
#' @return The totalSampleSize query indices
getQueryIndices <- function(X, y, queryIdx = NULL, nClass, totalSampleSize, querySize = 1,
                            models, uncertainty, initSampleSize, queryBagging = FALSE,
                            nBag = 1, verbose = TRUE) {
  message("\nStarting AL process to get queries indices")
  message("Checking input validity")
  source("uncertainties.R")
  source("utils.R")
  for(m in models) loadPackage(m)
  
  # Load allowedModelsList
  allowedModelsList <<- scan("resources/allowedModelsList.txt", what = "character", quiet = TRUE)
  
  # Load allowedUncertaintiesList
  allowedUncertaintiesList <<- scan("resources/allowedUncertaintiesList.txt", what = "character", quiet = TRUE)
  
  # Check input validity
  checkInputValidity(totalSampleSize, querySize, models, uncertainty, nrow(X), initSampleSize, queryBagging, nBag)
  
  # How many iterations to run
  numIter <- totalSampleSize / querySize
  
  # If queryIdx is NULL, this means we're querying for the first time - need to use random sample
  if (is.null(queryIdx)) {
    queryIdx <- sample(nrow(X), initSampleSize)
    numIter <- numIter - (initSampleSize/querySize)
  }
  
  # Stopping if the small starting random sample does not include all nClass labels
  # Run again or increase initSampleSize
  if (length(table(y[queryIdx])) != nClass) {
    stop("A model is about to get y values without all classes. Either numReports is too high or initSampleSize too low.")
  }
  
  message("Starting process")
  for (iter in 1:numIter) {
    
    if (verbose) message("Total no. of queries: ", length(queryIdx))
    
    # Create big data frame container for all classes predicted probabilities
    predDF <- data.frame(matrix(nrow = nrow(X[-queryIdx,]), ncol = length(models) * nBag * nClass))
    
    # Getting rid of zero variance features, creating temp train/test datasets
    sds <- apply(X[queryIdx,], 2, sd)
    tempTrainDF <- data.frame(y = y[queryIdx], X = X[queryIdx, which(sds > 0)])
    tempTestDF <- data.frame(y = y[-queryIdx], X = X[-queryIdx, which(sds > 0)])
    
    # Train models
    for (i in 1:length(models)) {
      if (verbose) message("Training model ", models[i])
      
      if (!queryBagging) {
        # Where to put nxC prob predictions in predDF data frame
        currPredLoc <- (nClass * (i - 1) + 1) : (nClass * i)
        predDF[, currPredLoc] <- fitModel(models[i], tempTrainDF, tempTestDF)
      } else {
        # If queryBagging = TRUE, for each model we bag nBag samples, each is treated as its own committee member
        for (b in 1:nBag) {
          queryIdxBagged <- sample(queryIdx, length(queryIdx), replace = TRUE)
          # For each bagged sample need to get rid of zero variance features, create temp datasets
          sds <- apply(X[queryIdxBagged,], 2, sd)
          tempTrainDF <- data.frame(y = y[queryIdxBagged], X = X[queryIdxBagged, which(sds > 0)])
          tempTestDF <- data.frame(y = y[-queryIdx], X = X[-queryIdx, which(sds > 0)])
          
          currPredLoc <- (nClass * ((i - 1) * nBag + (b - 1)) + 1) : (nClass * ((i - 1) * nBag + b))
          predDF[, currPredLoc] <- fitModel(models[i], tempTrainDF, tempTestDF)
        }
      }
    }
    
    # Choose next samples
    nextToQuery <- getNextQuery(predDF, querySize, uncertainty, length(models) * nBag, 1:nrow(X), queryIdx)
    
    # Add next samples to queryIdx
    queryIdx <- c(queryIdx, nextToQuery)
  }
  
  message("Finished, total no. of queries: ", length(queryIdx), "\n")
  
  # Return the query indices
  return(queryIdx)
}

#' Function for basic input validation
#' 
#' @param totalSampleSize The total no. of samples to query, a.k.a the Budget
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param models A character vector of model(s) to use (see allowedModelsList)
#' @param uncertainty Type of uncertainty (see allowedUncertaintiesList)
#' @param nData no. of samples in X
#' @param initSampleSize The no. of initial random samples to start with
#' @param queryBagging Logical indicating Whether to perform qury_bagging to create more committee members
#' @param nBag Number of bagged samples for each model in models
checkInputValidity <- function(totalSampleSize, querySize, models, uncertainty, nData,
                               initSampleSize, queryBagging, nBag) {
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
  
  # check that initSampleSize isn't larger or equal to totalSampleSize
  if (initSampleSize >= totalSampleSize) {
    stop("initSampleSize cannot be larger or equal than totalSampleSize")
  }
  
  # check that totalSampleSize - initSampleSize is a multiple of querySize
  # TODO: get rid of this
  if ((totalSampleSize - initSampleSize) %% querySize != 0) {
    stop("what's left of totalSampleSize after sampling initSampleSize is not a multiple of querySize")
  }
  
  # check that uncertainty type is consistent with the rest of parameters
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
}

#' Function for choosing the uncertainty function
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param uncertainty Type of uncertainty (see allowedUncertaintiesList)
#' @param  nModels Number of models
#' @param ids Original ids of the temp test set
#' @param queryIdx Current query indices vector
#' @return The next query indices to sample
getNextQuery <- function(pred, querySize, uncertainty, nModels, ids, queryIdx) {
  nextQueryIDs <- switch(uncertainty,
         leastConfident = leastConfident(pred, querySize),
         marginSampling = marginSampling(pred, querySize),
         entropy = entropy(pred, querySize),
         voteEntropy = voteEntropy(pred, querySize, nModels),
         voteMargin = voteMargin(pred, querySize, nModels))
  
  # Notice we return the indices of the dataset *without* the current queryIdx
  return(ids[-queryIdx][nextQueryIDs])
}