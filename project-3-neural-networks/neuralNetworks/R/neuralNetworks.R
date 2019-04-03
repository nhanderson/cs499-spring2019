#' NNetIterations
#' 
#' R function that calculates the precition function of a neural network using n-hidden units
#' 
#' @param X.mat feature matrix (n_observations x n_features)
#' @param y.vec label vector (n_observations x 1)
#' @param max.iterations int scalar > 1
#' @param step.size
#' @param n.hidden.units number of hidden units
#' @param is.train logical vector of size n_observations, TRUE if the observation is in the train set, FALSE for the validation set
#' 
#' @return list with pred.mat, V.mat, x.vec, predict(testX.mat)
#'
#' @examples
#'data(ozone, package="ElemStatLearn")
#'head(ozone)
#'X.unscaled.mat <- as.matrix(ozone[,-1])
#'head(X.unscaled.mat)
#'X.scaled <- scale(X.unscaled.mat)
#'head(X.scaled.mat)
#'y.vec <- ozone[,1]
#'n.hidden.units <- 2 #u
#'set.seed(1) # for reproducibility if needed
#'V <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units)
#'w <- rnorm(n.hidden.units)

#' loop
#' head( A <- X.scaled.mat %*% V) #1
#' sigmoid <- function(a){
#' 1/(1+exp(-a))
#' }
#' Z <- sigmoid(A) #2
#' head(b <- as.numeric(Z %*% w)) #3
#' head(delta.w <- b - y.vec) #4
#' head(A.deriv <- Z * (1-Z))
#' head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)) #5
#' head(grad.w <- t(Z) %*% delta.w /nrow(X.scaled.mat)) #6
#' head(grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)) #7

#' take a step
#' step.size <- 0.1
#' w <- w - step.size * grad.w
#' V <- V - step.size * grad.V
#' sum(abs(c(grad.w, as.numeric(grad.V))))
#' calc validation loss and stop when value goes up

NNetIterations <- function( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train ){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.integer(max.iterations)){
    stop("max.neighbors must be an integer")
  }
  if(!(is.integer(max.iterations) && max.iterations>1)){
    stop("max.iterations must be an integer greater than 1")
  }
  if(!is.integer(step.size)){
    stop("step.size must be an integer")
  }
  if( !is.integer(n.hidden.units) && n.hidden.units > 0){
    stop("n.hidden.units must be an integer greater than 0")
  }
  if(!is.vector(is.train)){
    stop("is.train must be a vector")
  }
  
  #' check if y.vec is binary or not
  #'
  #'
  #'
  #'

  X.scaled.mat <- scale(X.mat)
  V.mat <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units)
  w.vec <- rnorm(n.hidden.units)
  
  #' split is.train into train and validation set
  #' 
  #' 
  #' 
  #' 
  #' 

  
  #' loop through actual train data set
  for(n in c(1:ncol(is.train))){
    A <- X.scaled.mat %*% V   #' 1
    sigmoid <- function(a){  
      1/(1+exp(-a))
    }
    Z <- sigmoid(A)           #' 2
    b <- as.numeric(Z %*% w)  #' 3
    delta.w <- b - y.vec      #' 4
    A.deriv <- Z * (1-Z)    
    delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)      #' 5
    grad.w <- t(Z) %*% delta.w /nrow(X.scaled.mat)        #' 6
    grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)    #' 7
    
    #' while validation loss is decreasing --> do above loop on validation set?
    #' take a step
    w <- w - step.size * grad.w
    V <- V - step.size * grad.V
    sum(abs(c(grad.w, as.numeric(grad.V))))   #' divide by ncol(is.train) to get the average
  }
  
  
  #' not sure what to do with pred.mat and predict(testX.mat)
  return (list(pred.mat, V.mat, w.vec, predict(testX.mat)))
  
  
}
