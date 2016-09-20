getQueryIndices <- function(X, y, currTotalSampleSize, querySize, models, uncertainty, pca = FALSE, numPCs = NA) {
  
  # check that currTotalSampleSize isn't larger than total no. of unlabeled samples
  if (currTotalSampleSize > dim(X)[1]) {
    stop("currTotalSampleSize cannot be larger than total n")
  }
  
  # check that querySize isn't larger than currTotalSampleSize
  if (querySize > currTotalSampleSize) {
    stop("querySize cannot be larger than currTotalSampleSize")
  }
  
  # if pca was chosen:
  if (pca) {
    # check that numPCs is specified
    if (is.na(numPCs)) {
      stop("Please specify Number of PCs")
    }
    # check that numPCs is lower than features dimension
    if (numPCs > dim(X)[2]) {
      stop("Number of PCs cannot be larger than no. of features")
    }
    
    # Perform PCA
    X <- prcomp(X)$rotation[, 1:numPCs]
  }
  
  numIter <- ceiling(currTotalSampleSize / querySize)
  queryIdx <- numeric()
  
  for (iter in 1:numIter) {
    # If first iteration: choose initial querySize samples
    if (iter == 1) {
      queryIdx <- sample(dim(X)[1], querySize)
    }
    
    # Train models
    
    # Get predictions
    
    # Choose next samples
    nextToQuery <- NULL
    
    # Add next samples to queryIdx
    queryIdx <- c(queryIdx, nextToQuery)
  }
  
  # In case currTotalSampleSize %% querySize != 0 we have a little too many observations
  queryIdx <- queryIdx[1:currTotalSampleSize]
  
  # Return the query indices
  return(queryIdx)
}

main <- function(totalSampleSize = 500, querySize = 1, models, uncertainty, pca = FALSE, numPCs = NA) {
  # Load MNIST datasets
  load_mnist()
  
  # Approve input
  # check that all models are in allowedModelsList
  if (any(!models %in% allowedModelsList)) {
    stop("You have specified a model not in the allowedModelsList")
  }
  
  # check that uncertainty is in the allowedUncertaintiesList
  if (!uncertainty %in% allowedUncertaintiesList) {
    stop("You have specified a uncertainty not in the allowedUncertaintiesList")
  }
  
  # check that totalSampleSize isn't larger than total no. of unlabeled samples
  if (totalSampleSize > dim(X)[1]) {
    stop("totalSampleSize cannot be larger than total n")
  }
  
  # check that querySize isn't larger than totalSampleSize
  if (querySize > totalSampleSize) {
    stop("querySize cannot be larger than totalSampleSize")
  }
  
  # Iterate between 50 and 500
  for (currTotalSampleSize in seq(floor(totalSampleSize/10),totalSampleSize,floor(totalSampleSize/10))) {
    # Get query indices
    queryIdx <- getQueryIndices(X, y, currTotalSampleSize, querySize, models, uncertainty, pca = FALSE, numPCs = NA)
    
    # Perform final model(s) on query
    
    
    # Get AL test data performance
    
    
    # Perform final model(s) on random totalSampleSize from train
    
    
    # Get PL test data performance
    
  }
  
  
  # Save results
  
  # Graph results and save graphs
  
}