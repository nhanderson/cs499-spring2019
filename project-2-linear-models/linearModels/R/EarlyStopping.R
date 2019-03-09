#' LMSquareLossIterations function
#' 
#' R function that wraps the C++ code.
#'
#' @param X.mat numeric train featrue matrix [n x p]
#' @param y.vec numeric train label vector [n]
#' @param max.iteratons scalar integer greater than 1, max number of iterations
#' @param step.size scalar integer, step size
#'
#' @return matrix of weight vectors 
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
#' zip.train[test.i, 1]
#' 

LMSquareLossIterations <- function(X.mat, y.vec, max.iterations, step.size){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.integer(max.iterations)){
    stop("max.neighbors must be an integer")
  }
  if(!(max.iterations>1)){
    stop("max.iterations must be an integer greater than 1")
  }
  if(!is.integer(step.size)){
    stop("step.size must be an integer")
  }
  
  scaledX.mat <- scale( X.mat, center = FALSE, scale = apply( X.mat, 2, na.rm = TRUE) )
  
  result<- .C("LMSquareLossIterations_interface", 
              as.integer(nrow(X.mat)),  #' n_train
              as.integer(nrow(X.mat)),  #' n_test
              as.integer(ncol(X.mat)),  #' n_features
              as.integer(max.iterations),  #' max_iterations
              as.integer(step.size),   #' step_size
              as.double(scaledX.mat),  #' train_input_ptr
              as.double(y.vec),  #' train_output_ptr
              weight_mat = double(ncol(X.mat) * max.iterations)  #' predictions_output_ptr
              )
  weight.mat <- matrix(result$weight_mat, ncol(X.mat), max_iterations)
  unscaled.weight.mat <- unscale( weight.mat, X.mat )
}

#' LMLogisticLossIterations function
#' 
#' R function that 
#'
#' @param X.mat numeric train feature matrix [n x p]
#' @param y.vec numeric train label vector [n]
#' @param max.iteratons scalar integer greater than 1, max number of iterations
#' @param step.size scalar integer, step size
#'
#' @return matrix of weight vectors 
#' @export 
#'
#' @examples
#' data(spam, package = "ElemStatLearn")
#' X.mat <- as.matrix(spam[, 1:57])
#' y.vec <- ifelse(spam$spam=="spam", 1, 0)
#' LMLogisticLossIterations(X.mat, y.vec, as.integer(3), 0.1)

LMLogisticLossIterations <- function(X.mat, y.vec, max.iterations, step.size){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.integer(max.iterations)){
    stop("max.iterations must be an integer")
  }
  if(!(max.iterations>1)){
    stop("max.iterations must be an integer greater than 1")
  }
  if(!is.numeric(step.size)){
    stop("step.size must be numeric")
  }
  
  # Scale feature matrix
  scaled.X.mat  <- scale(X.mat)
  
  # Function
  scaled.W.mat <- matrix(0, nrow(X.mat), max.iterations)
  
  result <- .C("LMLogisticLoss_interface",
               as.integer(nrow(X.mat)),  #' n_train
               as.integer(ncol(X.mat)),  #' n_features
               as.double(scaled.X.mat),  #' train_input_ptr
               as.double(y.vec),  #' train_output_ptr
               as.double(as.vector(matrix(0,nrow=ncol(X.mat)))),  #' weight_ptr
               weight_mat = double(ncol(X.mat)))   #' output_ptr
  
  scaled.W.loss.vec <- result$weight_mat
  scaled.W.mat[, 1] <- as.vector(matrix(0,nrow=ncol(X.mat))) - step.size * scaled.W.loss.vec
  
  for(iteration in seq(2, max.iterations, by=1)){
    
    result <- .C("LMLogisticLoss_interface",
                 as.integer(nrow(X.mat)),  #' n_train
                 as.integer(ncol(X.mat)),  #' n_features
                 as.double(scaled.X.mat),  #' train_input_ptr
                 as.double(y.vec),  #' train_output_ptr
                 as.double(scaled.W.mat[, iteration-1]),  #' weight_ptr
                 weight_mat = double(ncol(X.mat))) #' output_ptr
    
    scaled.W.loss.vec <- result$weight_mat
    scaled.W.mat[, iteration] <- scaled.W.mat[, iteration-1] - step.size * scaled.W.loss.vec
  }
  
  # W.mat <- unscale( scaled.W.mat, X.mat )
  return(scaled.W.mat)
}


#' LMLogisticLossEarlyStoppingCV function
#' 
#' R function that 
#'
#' @param X.mat numeric train featrue matrix [n x p]
#' @param y.vec numeric train label vector [n]
#' @param fold.vec numeric fold id vector [n] 
#' @param max.iterations scalar integer greater than 1, max number of iterations
#'
#' @return mean.validation.loss
#' @return mean.train.loss.vec 
#' @return selected.steps
#' @return weight.vec  the weight vector found by using gradient descent with selected.steps on the whole training data set.
#' @return predict(testX.mat) a function that takes a test features matrix and returns a vector of predictions 
#' @export 
#'
#' @examples
#' 
LMLogisticLossEarlyStoppingCV <- function(X.mat, y.vec, fold.vec, max.iterations){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.vector(fold.vec)){
    stop("fold.vec must be a vector")
  }
  if(!is.integer(max.iterations)){
    stop("max.neighbors must be an integer")
  }
  if(!(max.iterations>1)){
    stop("max.iterations must be an integer greater than 1")
  }

  
  # should use K-fold cross-validation based on the fold IDs provided in fold.vec
  
  # for each train/validation split, use LM___LossIterations to compute a sequence of models on the train data, 
  # then compute the validation loss of each model.
  
  # compute mean.validation.loss.vec, which is a vector (with max.iterations elements) of mean validation loss over all K folds.
  
  # minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  
  # finally use LM__LossIterations(max.iterations=selected.steps) on the whole training data set.
  
  # Output a list with the following named elements:

}
