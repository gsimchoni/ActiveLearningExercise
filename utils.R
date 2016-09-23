#' Function to choose the right package to upload
#' If you don't have these use "install.packages(package_name)"
#' 
#' @param model The model to load
loadPackage <- function(model) {
  switch(model,
         knn = suppressMessages(library(caret)),
         gbm = suppressMessages(library(gbm)),
         rf = suppressMessages(library(randomForest)),
         svm = suppressMessages(library(e1071)),
         rpart = suppressMessages(library(rpart)),
         bag = suppressMessages(library(ipred)))
}

#' Wrapper function to route the model fit and prediction to the desired format
#' TODO: this is cumbersome, maybe go back to the 'caret' framework
#' 
#' @param model The model string
#' @param tempTrainDF Temporary train dataset
#' @param tempTestDF Temporary test dataset
#' @return The nXC predicted probabilities data frame
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
  # SVM mixes up labels order for some reason, need to get it right
  return(pred[, order(colnames(pred))])
}
