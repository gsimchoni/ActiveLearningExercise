# ActiveLearningExercise
An Exercise in Active Learning Implementation in R on MNIST dataset

## How to run
If you already have R installed, continue from 3:

1. Get R: https://www.r-project.org/
2. For your own good get RStudio IDE: https://www.rstudio.com/
3. Get the MNIST data: http://yann.lecun.com/exdb/mnist/
4. Clone this repository to local
5. gunzip each of the 4 MNIST files into a folder names 'mnist', inside the 'ActiveLearningExercise' which you have cloned
6. In R (or R Studio) install the necessary packages if you don't have them, e.g. `install.packages("caret")`
    * `caret`
    * `gbm`
    * `randomForest`
    * `e1071`
    * `ipred`
    * `rpart`
7. In R (or R Studio) set the working directory to the 'ActiveLearningExercise', e.g. `setwd("ActiveLearningExercise")`
8. Import the `MNISTWrapper.R` script by `source(MNISTWrapper.R)`
9. Try running:

`runMNISTActiveLearning <- function(totalSampleSize = 200, querySize = 10, models = c("rf", "gbm"), uncertainty = "voteEntropy", numReports = 10, initSampleSize = 100, finalModel = "rf")`
