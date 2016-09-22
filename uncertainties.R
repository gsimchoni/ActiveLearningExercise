leastConfident <- function(pred, querySize) {
  confidence <- 1 - apply(pred, 1, max)
  confidence.order <- order(confidence, decreasing = TRUE)
  return(confidence.order[1:querySize])
}

marginSampling <- function(pred, querySize) {
  margin <- apply(pred, 1, calculateMargin)
  margin.order <- order(margin)
  return(margin.order[1:querySize])
}

entropy <- function(pred, querySize) {
  entropy <- apply(pred, 1, calculateEntropy)
  entropy.order <- order(entropy, decreasing = TRUE)
  return(entropy.order[1:querySize])
}

voteEntropy <- function(pred, querySize, nModels) {
  predictedClasses <- t(apply(pred, 1, getPredictedClasses, nModels))
  
  predictedClassesTabbed <- apply(predictedClasses, 1, table)
  
  vEntropy <- sapply(predictedClassesTabbed, function(x) -sum((x/nModels) * log(x/nModels)))
  
  vEntropy.order <- order(vEntropy, decreasing = TRUE)
  return(vEntropy.order[1:querySize])
}

voteMargin <- function(pred, querySize, nModels) {
  predictedClasses <- t(apply(pred, 1, getPredictedClasses, nModels))
  
  predictedClassesTabbed <- apply(predictedClasses, 1, table)
  
  margin <- sapply(predictedClassesTabbed, calculateVoteMargin)
  
  margin.order <- order(margin)
  return(margin.order[1:querySize])
}


calculateMargin <- function(x) {
  o <- order(x, decreasing = TRUE)
  return(x[o[1]] - x[o[2]])
}

calculateEntropy <- function(x) {
  return(-sum(x * log(x)))
}

getPredictedClasses <- function(x, nModels) {
  x.chunked <- split(as.numeric(x), cut(seq_along(x), nModels, labels = FALSE))
  return(sapply(x.chunked, which.max))
}

calculateVoteMargin <- function(x) {
  if (length(x) == 1) return(x)
  o <- order(x, decreasing = TRUE)
  return(x[o[1]] - x[o[2]])
}
