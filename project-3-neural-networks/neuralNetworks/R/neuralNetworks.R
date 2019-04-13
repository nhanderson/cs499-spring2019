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
#'X.mat <- as.matrix(ozone[,-1])
#'y.vec <- ozone[,1]
#'n.hidden.units <- 2L #u
#'max.iterations <- 5L
#'n.folds <- 5
#'unique.folds <- 1:n.folds
#'set.seed(1)
#'step.size <- 0.1
#'fold.vec <- sample(rep(unique.folds, l=nrow(X.mat)))
#'validation.fold <- 1
#'is.train <- fold.vec != validation.fold
#'NNetIterations(X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train)

NNetIterations <- function( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train ){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.numeric(y.vec)){
    stop("y.vec must be a vector")
  }
  if( !(is.integer(max.iterations) && max.iterations>1) ){
    stop("max.iterations must be an integer greater than 1")
  }
  if( !(is.numeric(step.size) && step.size > 0 )) {
    stop("step.size must be numeric and greater than 0")
  }
  if( !(is.integer(n.hidden.units) && n.hidden.units > 0)){
    stop("n.hidden.units must be an integer greater than 0")
  }
  if(!is.logical(is.train)){
    stop("is.train must be a logical vector")
  }
  
  X.scaled.mat <- scale(X.mat)
  V.mat <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units)
  w.vec <- rnorm(n.hidden.units)
  
  # split is.train into train and validation set
  X.train <- X.mat[is.train, ]
  y.train <- y.vec[is.train]
  
  pred.mat <- matrix(0, length(y.vec), max.iterations)
  
  # convert binary vector into scaled y.tilde in {-1,1}
  is.binary <- all(y.vec %in% c(0,1))
  if( is.binary ){
    y.tilde <- ifelse(y.vec == 1, 1, -1)
  }

  # loop through actual train data set
  for(n in c(1:max.iterations)){
    A <- X.scaled.mat %*% V.mat   #' 1
    sigmoid <- function(a){  
      1/(1+exp(-a))
    }
    Z <- sigmoid(A)           #' 2
    b <- as.numeric(Z %*% w.vec)  #' 3
    
    if( is.binary ){
      delta.w <- -y.tilde %*% sigmoid(-y.tilde %*% b) 
    }
    else {
      delta.w <- b - y.vec      #' 4
    }
    
    A.deriv <- Z * (1-Z)    
    delta.v <- diag(delta.w) %*% A.deriv %*% diag(as.numeric(w.vec))     #' 5
    
    grad.w <- t(Z) %*% delta.w /nrow(X.scaled.mat)        #' 6
    grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)    #' 7

    # take a step
    w.vec <- w.vec - step.size * grad.w
    V.mat <- V.mat - step.size * grad.V
    
    prediction <- t(w.vec) %*% sigmoid(t(X.scaled.mat %*% V.mat))
    pred.mat[, n] <- prediction
  }

  # unscale predictions
  V.orig <- V.mat/attr(X.scaled.mat, "scaled:scale")
  b.orig <- -t(V.mat/attr(X.scaled.mat, "scaled:scale")) %*% attr(X.scaled.mat, "scaled:center")
  
  V.with.intercept <- rbind(intercept=as.numeric(b.orig), V.orig)
  
  # prediction function that takes an unsclaed X matrix
  predict <- function(X.unscaled){
    A.mat <- cbind(1, X.unscaled) %*% V.with.intercept
    sigmoid(A.mat) %*% w
  }
  
  return (list(pred.mat=pred.mat, V.mat = V.with.intercept, w.vec = w.vec, predict=predict))
}

#' NNetEarlyStoppingCV
#' 
#' R function that calculates the precition function of a neural network using n-hidden units
#' 
#' @param X.mat feature matrix (n_observations x n_features)
#' @param y.vec label vector (n_observations x 1)
#' @param fold.vec fold ID vector (n_observations x 1)
#' @param max.iterations int scalar > 1
#' @param step.size
#' @param n.hidden.units number of hidden units
#' 
#' @return list with pred.mat, V.mat, x.vec, predict(testX.mat), mean.validation.loss, mean.train.loss.vec, selected.steps
#'
#' @examples
#'data(ozone, package="ElemStatLearn")
#'X.mat <- as.matrix(ozone[,-1])
#'y.vec <- ozone[,1]
#'n.hidden.units <- 2L #u
#'max.iterations <- 100L
#'n.folds <- 5L
#'unique.folds <- 1:n.folds
#'set.seed(1)
#'step.size <- 0.1
#'fold.vec <- sample(rep(unique.folds, l=nrow(X.mat)))
#'NNetEarlyStoppingCV(X.mat, y.vec, fold.vec, max.iterations, step.size, n.hidden.units)
NNetEarlyStoppingCV <- function( X.mat, y.vec, fold.vec, max.iterations, step.size, n.hidden.units ){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.vector(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!(length(y.vec)==nrow(X.mat))){
    stop("y.vec must have the same length of X.mat rows")
  }
  if(!is.vector(fold.vec)){
    stop("fold.vec must be a vector")
  }
  if(!(length(fold.vec)==nrow(X.mat))){
    stop("fold.vec must have the same length of X.mat rows")
  }
  if(!is.integer(max.iterations)){
    stop("max.neighbors must be an integer")
  }
  if(!((is.integer(max.iterations) && max.iterations > 1))){
    stop("max.iterations must be an integer greater than 1")
  }
  if(!is.numeric(step.size)){
    stop("step.size must be a numeric value")
  }
  if(!(is.numeric(step.size) && step.size > 0)){
    stop("step.size must be a numeric value greater than 0")
  }
  if(!(is.integer(n.hidden.units) && n.hidden.units > 0)){
    stop("n.hidden.units must be an integer greater than 0")
  }
  
  # should use K-fold cross-validation based on the fold IDs provided in fold.vec
  fold.ids <- unique(fold.vec)
  
  # determine if v.vec is binary to be used during loss calculation
  is.binary <- all(y.vec %in% c(0,1))
  
  # initalize loss matrix to store loss values for each fold
  fold.validation.loss.mat <- matrix(0, length(fold.ids), max.iterations)
  fold.train.loss.mat <- matrix(0, length(fold.ids), max.iterations)
  
  for(fold.i in fold.ids){
    is.train <- fold.vec != fold.i
    is.validation <- fold.vec == fold.i
    
    # For each train/validation split, use NNetIterations to compute the predictions for all observations
    fold.result <- NNetIterations( X.mat, y.vec, max.iterations, step.size, n.hidden.units, is.train )
    fold.pred.mat <- fold.result$pred.mat
    
    # Calculate the loss for the fold 
    # use the square loss for regression and the 01-loss for binary classification
    
    fold.loss <- if(is.binary){
      log(1+exp(-y.vec %*% fold.pred.mat))
    }else{
      abs(fold.pred.mat - y.vec)^2 / nrow(X.mat) 
    }
    
    fold.validation.loss <- fold.loss[is.validation,]
    fold.train.loss <- fold.loss[is.train,]
    
    # store fold loss in loss matrix
    fold.validation.loss.mat[fold.i, ] <- colSums(fold.validation.loss)
    fold.train.loss.mat[fold.i, ] <- colSums(fold.train.loss)
  }
  # Compute mean.validation.loss.vec, which is a vector (with max.iterations elements) of mean validation loss over all K
  # folds (use the square loss for regression and the 01-loss for binary classification).
  mean.validation.loss.vec <- colMeans(fold.validation.loss.mat)
    
  # TODO compute mean.train.loss.vec, analogous to above but for the train data.
  mean.train.loss.vec <- colMeans(fold.train.loss.mat)
    
  # minimize the mean validation loss to determine selected.steps, the optimal number of steps/iterations.
  selected.steps <- which.min(mean.validation.loss.vec)
  
  # TODO finally use NNetIterations(max.iterations=selected.steps) on the whole training data set.
  is.train <- rep(TRUE, nrow(X.mat))
  final.result <- NNetIterations( X.mat, y.vec, selected.steps, step.size, n.hidden.units, is.train )
  
  # Output the same list from NNetIterations
  # pred.mat, n_observations x max.iterations matrix of predicted values
  # V.mat final weight matrix (n_features+1 x n.hidden.units). The first row of V.mat should be the intercept terms.
  # w.vec final weight vector (n.hidden.units+1). The first element of w.vec should be the intercept term.
  # predict(testX.mat), a function that takes an unscaled test feature matrix and returns a vector of predictions
  # mean.validation.loss, mean.train.loss.vec (for plotting train/validation loss curves)
  # selected.steps
  
  final.result$mean.validation.loss <- mean.validation.loss.vec
  final.result$mean.train.loss.vec <-mean.train.loss.vec
  final.result$selected.steps <- selected.steps
  return(final.result)

}
