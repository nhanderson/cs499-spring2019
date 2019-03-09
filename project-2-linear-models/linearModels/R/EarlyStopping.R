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
#' data(prostate, package="ElemStatLearn")
#' x <- prostate[1, ]
#' y <- prostate[, 'lpsa']
#' LMSquareLossIterations(x ,y, as.integer(12), as.integer(0.5))
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
  
  scaled.X.mat <- scale( X.mat )
  
  result<- .C("LMSquareLossIterations_interface", 
              as.integer(nrow(X.mat)),  #' n_train
              as.integer(nrow(X.mat)),  #' n_test
              as.integer(ncol(X.mat)),  #' n_features
              as.integer(max.interations),  #' max_iterations
              as.integer(step.size),   #' step_size
              as.double(scaled.X.mat),  #' train_input_ptr
              as.double(y.vec),  #' train_output_ptr
              weight.mat = double(ncol(X.mat) * max.iterations)  #' predictions_output_ptr
              )
  
  unscaled.weight.mat <- (result.list$weight.mat * y.vec)/attr(result.list$weight.mat, "scaled:scale") 
                                          - (y.vec * attr(result.list$weight.mat, "scaled:center"))/attr(result.list$weight.mat, "scaled:scale")
  scaled.weight.mat <- matrix(unscaled.weight.mat, ncol(X.mat), max.iterations)
}

#' LMLogisticLossIterations function
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
#' data(spam, package="ElemStatLearn")
#' x <- prostate[1, ]
#' y <- prostate[, 'spam']
#' LMSquareLossIterations(x ,y, as.integer(12), as.integer(0.5))
#'

LMLogisticLossIterations <- function(X.mat, y.vec, max.iterations, step.size){
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
  
  scaled.X.mat <- scale( X.mat )
  
  result<- .C("LMLogisticLossIterations_interface", 
              as.integer(nrow(X.mat)),  #' n_train
              as.integer(nrow(X.mat)),  #' n_test
              as.integer(ncol(X.mat)),  #' n_features
              as.integer(max.interations),  #' max_iterations
              as.integer(step.size),   #' step_size
              as.double(scaled.X.mat),  #' train_input_ptr
              as.double(y.vec),  #' train_output_ptr
              weight.mat = double(ncol(X.mat) * max.iterations)  #' predictions_output_ptr
  )
  
  unscaled.weight.mat <- (result.list$weight.mat * y.vec)/attr(result.list$weight.mat, "scaled:scale") 
  - (y.vec * attr(result.list$weight.mat, "scaled:center"))/attr(result.list$weight.mat, "scaled:scale")
  scaled.weight.mat <- matrix(unscaled.weight.mat, ncol(X.mat), max.iterations)
}
