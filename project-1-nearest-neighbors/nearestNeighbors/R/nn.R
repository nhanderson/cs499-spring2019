#' nearest neighbors algorithm
#' 
#' R function that wraps the C++ code.
#'
#' @param x.mat numeric train featrue matrix [n x p]
#' @param y.vec numeric train label vecotr [n], either all 0/1 for binary classificiation, or other real numbers for regression (multi-class classification not supported)
#' @param testx.vec numeric test feature vector [p]
#' @param max.neighbors scalar integer, max number of neighbors
#'
#' @return numeric vector of size man.neighbors, predictions from 1 to max.neighbors
#' @export 
#'
#' @examples
#' data(zip.train, package="ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' NN1toKmaxPredict(x ,y, testx , as.integer(3))
#' zip.train[test.i, 1]
#' 

NN1toKmaxPredict <- function(x.mat, y.vec, testx.vec, max.neighbors){
  if(!is.matrix(x.mat)){
    stop("x.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.vector(testx.vec)){
    stop("testx.vec must be a vector")
  }
  if(!is.integer(max.neighbors)){
    stop("max.neighbors must be an integer")
  }
  if(length(y.vec)!=nrow(x.mat)){
    stop("The length of y.vec must match the same number of rows as x.mat")
  }
  if(length(testx.vec)!=ncol(x.mat)){
    stop("The length of y.vec must match the same number of rows as x.mat")
  }
  result.list <- .C("NN1toKmaxPredict_interface", 
                    as.integer(nrow(x.mat)), #' n_train_observations
                    as.integer(nrow(x.mat)), #' n_test_observations
                    as.integer(ncol(x.mat)), #' n_features
                    as.integer(max.neighbors), #' max_neighbors
                    as.double(x.mat), #' train_in_ptr
                    as.double(y.vec), #' train_out_ptr
                    as.double(testx.vec), #' test_in_ptr
<<<<<<< HEAD
                    integer(max.neighbors), #' predictions_out_ptr
=======
                    predictions=double(max.neighbors), #' predictions_out_ptr
>>>>>>> 43d95826e65e0b2f90ec4d1749d2535c1cf07748
                    PACKAGE="nearestNeighbors")
  result.list$predictions
}


#' cross validation of nearest neighbors algorithm
#' 
#'
#' @param x.mat numeric train feature matrix [n x p]
#' @param y.vec numeric train label vecotr [n], either all 0/1 for binary classificiation, or other real numbers for regression (multi-class classification not supported)
#' @param max.neighbors scalar integer, max number of neighbors
#' @param fold.vec vector of fold ID numbers, will be randonly assigned if not specified
#' @param n.folds integer specifiying number of folds to perform
#'
#' @return list containing x.mat, y.vec, train.loss.mat, validation.loss.mat, train.loss.vec, validation.loss.vec, selected.neighbors, predict(testX.mat))
#' @export 
#'
#' @examples
#' data(zip.train, package="ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' NNLearnCV(x ,y)
#' 

NNLearnCV <-function(x.mat, y.vec, max.neighbors=as.integer(30), fold.vec=NULL, n.folds=as.integer(5)){
  # If folds is null, randomly assign fold values to each row between 1 and n.folds
  if(is.null(fold.vec)){
    fold.vec <- sample(rep(1:n.folds, l=nrow(x.mat)))
  }
  
  # Perform type checking and size checking on input
  if(length(fold.vec)!=length(y.vec)){
    stop("fold.vec must be the same size as y.vec")
  }
  if(!is.matrix(x.mat)){
    stop("x.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(length(y.vec)!=nrow(x.mat)){
    stop("The length of y.vec must match the same number of rows as x.mat")
  }
  if(!is.integer(max.neighbors)){
    stop("max.neighbors must be an integer")
  }
  if(!is.integer(n.folds)){
    stop("n.folds must be an integer")
  }
  
  for(fold.i in unique(n.folds)){
    is.train <- fold.vec != fold.i
    is.validation <- fold.vec == fold.i
    for(prediction.set.name in c("train", "validation")){
      prediction.mat <- NN1toKmaxPredict(x.mat[is.train,], y.vec[is.train], x.mat[is.validation,], max.neighbors)
      loss.mat <- if(y.vec[is.train] %in% c(0,1)){
        ifelse(prediction.mat>0.5, 1, 0) != y.vec #zero-one loss for binary classification.
      }
      else {
        (prediction.mat-y.vec)^2 #square loss for regression.
      }
      train.or.validation.loss.mat[, fold.i] <- colMeans(loss.mat)
    }
  }
  
  train.loss.vec <- colMeans(train.loss.mat)
  validation.loss.vec <- colMeans(validation.loss.mat)
  selected.neighbors <- which.min(validation.loss.vec)
  
  predict=function(testX.mat){
    
  }
  
  result.list <- list(x.mat, 
                      y.vec=y.vec, #' training data.
                      train.loss.mat=train.loss.mat, #' matrix of loss values for each fold and number of neighbors
                      validation.loss.mat=validation.loss.mat, #' matrix of loss values for each fold and number of neighbors
                      train.loss.vec=train.loss.vec, #' vectors with max.neighbors elements: mean loss over all folds for training data
                      validation.loss.vec=validation.loss.vec, #' vectors with max.neighbors elements: mean loss over all folds for validation data
                      selected.neighbors=selected.neighbors, #' number of neighbors selected by minimizing the mean validation loss
                      predict(testX.mat))=predict #' a function that takes a matrix of inputs/features and returns a vector of predictions
  result.list

}
