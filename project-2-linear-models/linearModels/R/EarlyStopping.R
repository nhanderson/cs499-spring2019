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
#' NN1toKmaxPredict(x ,y, testx , as.integer(3))
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
  
   <- scale( X.mat, center = FALSE, scale = apply( X.mat, 2, na.rm = TRUE) )
  
  result<- .C("LMSquareLossIterations_interface", 
              as.integer(nrow(X.mat)),  #' n_train
              as.integer(nrow(X.mat)),  #' n_test
              as.integer(ncol(X.mat)),  #' n_features
              as.integer(max.interations),  #' max_iterations
              as.integer(step.size),   #' step_size
              as.double(scaledX.mat),  #' train_input_ptr
              as.double(y.vec),  #' train_output_ptr
              weight_mat = double(ncol(X.mat) * max.iterations)  #' predictions_output_ptr
              )
  weight.mat <- matrix(result.list$weight_mat, ncol(X.mat), max_iterations)
  unscaled.weight.mat <- unscale( weight.mat, X.mat )
}


