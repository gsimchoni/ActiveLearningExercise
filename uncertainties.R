#' Least Confident uncertainty metric
#' See Burr Settles Review: http://burrsettles.com/pub/settles.activelearning.pdf
#' 
#' @author gsimchoni
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @return Next querySize indices
leastConfident <- function(pred, querySize) {
  confidence <- 1 - apply(pred, 1, max)
  confidence.order <- order(confidence, decreasing = TRUE)
  return(confidence.order[1:querySize])
}

#' Margin Sampling uncertainty metric
#' See Burr Settles Review: http://burrsettles.com/pub/settles.activelearning.pdf
#' 
#' @author gsimchoni
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @return Next querySize indices
marginSampling <- function(pred, querySize) {
  margin <- apply(pred, 1, calculateMargin)
  margin.order <- order(margin)
  return(margin.order[1:querySize])
}

#' Entropy uncertainty metric
#' See Burr Settles Review: http://burrsettles.com/pub/settles.activelearning.pdf
#' 
#' @author gsimchoni
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @return Next querySize indices
entropy <- function(pred, querySize) {
  entropy <- apply(pred, 1, calculateEntropy)
  entropy.order <- order(entropy, decreasing = TRUE)
  return(entropy.order[1:querySize])
}

#' Vote Entropy committee uncertainty metric
#' See Burr Settles Review: http://burrsettles.com/pub/settles.activelearning.pdf
#' 
#' @author gsimchoni
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param nModels Number of models in committee
#' @return Next querySize indices
voteEntropy <- function(pred, querySize, nModels) {
  predictedClasses <- t(apply(pred, 1, getPredictedClasses, nModels))
  
  predictedClassesTabbed <- apply(predictedClasses, 1, table)
  
  vEntropy <- sapply(predictedClassesTabbed, function(x) -sum((x/nModels) * log(x/nModels)))
  
  vEntropy.order <- order(vEntropy, decreasing = TRUE)
  return(vEntropy.order[1:querySize])
}

#' Vote Margin committee uncertainty metric
#' This is my own invention based on the concept of margin sampling
#' 
#' @author gsimchoni
#' 
#' @param pred The big data frame container for all classes predicted probabilities
#' @param querySize How many samples to query in each iteration (default is 1)
#' @param nModels Number of models in committee
#' @return Next querySize indices
voteMargin <- function(pred, querySize, nModels) {
  predictedClasses <- t(apply(pred, 1, getPredictedClasses, nModels))
  
  predictedClassesTabbed <- apply(predictedClasses, 1, table)
  
  margin <- sapply(predictedClassesTabbed, calculateVoteMargin)
  
  margin.order <- order(margin)
  return(margin.order[1:querySize])
}

#' Margin is the gap between two highest predicted class probabilities
calculateMargin <- function(x) {
  o <- order(x, decreasing = TRUE)
  return(x[o[1]] - x[o[2]])
}

#' Entropy is minus sum of all class probabilities p*(log(p))
calculateEntropy <- function(x) {
  return(-sum(x * log(x)))
}

# Get for each model the predicted class, with highest probability
getPredictedClasses <- function(x, nModels) {
  x.chunked <- split(as.numeric(x), cut(seq_along(x), nModels, labels = FALSE))
  return(sapply(x.chunked, which.max))
}

# 'Vote Margin' is the gap between the two highest votes
calculateVoteMargin <- function(x) {
  if (length(x) == 1) return(x)
  o <- order(x, decreasing = TRUE)
  return(x[o[1]] - x[o[2]])
}
