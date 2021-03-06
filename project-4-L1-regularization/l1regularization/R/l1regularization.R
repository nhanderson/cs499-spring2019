#' LinearModelL1 
#' 
#' R function that calculates the L1 Regularization
#' 
#' @param X.scaled.mat scaled feature matrix (n_observations x n_features)
#' @param y.vec label vector (n_observations x 1)
#' @param penalty non-negative numeric scalar
#' @param opt.thresh positive numeric scalar, threshold on the sub-differential optimality criterion
#' @param initial.weight.vec weight vector (n_observations x 1)
#' @param step.size int scalar > 1
#' 
#' @return w.opt optimal weight vector (n_observations+1 x 1)
#' 
#' @export
#'
#' @examples
#' data(zip.train, package = "ElemStatLearn")
#' all.y.vec <- zip.train[,1]
#' is.01<- all.y.vec %in% c(0,1)
#' y.vec <- all.y.vec[is.01]
#' X.mat <- zip.train[is.01, -1]
#' n.folds <- 5
#' fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
#' validation.fold <- 1
#' is.train <- fold.vec != validation.fold
#' is.validation <- fold.vec == validation.fold
#' X.train <- X.mat[is.train,]
#' y.train <- y.vec[is.train]
#' X.scaled.mat <- scale(X.train)
#' X.filtered <- X.scaled.mat[, attr(X.scaled.mat, "scaled:scale") != 0]
#' opt.thresh <- 0.01
#' initial.weight.vec <- rep( 0, l=ncol(X.filtered) +1)
#' step.size <- 0.1
#' penalty <- 1
#' LinearModelL1( X.filtered, y.train, penalty, opt.thresh, initial.weight.vec, step.size )

LinearModelL1 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ){
  if(!is.matrix(X.scaled.mat)){
    stop("X.scaled.mat must be a matrix")
  }
  if(!is.numeric(y.vec)){
    stop("y.vec must be a vector")
  }
  if( !(is.numeric(penalty) && penalty >= 0) ){
    stop("penalty must be numeric and greater than or equal to zero")
  }
  if( !(is.numeric(opt.thresh) && penalty > 0) ){
    stop("penalty must be numeric and greater than zero")
  }
  if( !is.numeric(initial.weight.vec)){
    stop("initial.weight.vec must be a vector")
  }
  if( !(is.numeric(step.size) && step.size > 0)) {
    stop("step.size must be numeric and greater than zero")
  }

  #initialize w w/ intercept col, first term is bias/intercept
  X.int <- cbind(1, X.scaled.mat)
  w.vec <- initial.weight.vec
  
  sigmoid <- function(z){
    1/(1+exp(-z))
  }
  
  soft <- function(x,lambda){
    sign(x) * posPart(abs(x)-lambda)
  }

  sign <- function(x){
    if(x > 0){ 1 } 
    else if (x < 0) { -1 } 
    else { 0 }
  }
  
  posPart <- function(x) {
    ifelse( x > 0, x, 0 )
  }
  
  is.01 <- all(y.vec %in% c(0,1))
  
  if(is.01){
    y.tilde <- ifelse(y.vec==1,1,-1)
  }
  
  is.opt <- function(d.vec, w.vec) {
    result.vec <- rep( 0, length(w.vec) )
    for(index in 1:length(w.vec)){
      if(w.vec[index] != 0) {
        result.vec[index] <- abs(d.vec[index] - (sign(w.vec[index])))
      } else {
        result.vec[index] <- abs(d.vec[index])
      }
    }
    ifelse(length(result.vec[result.vec < opt.thresh]) == length(result.vec), TRUE, FALSE)
  }
  
  grad.loss <- function(X.int, w.vec){
    
    pred.vec <- X.int %*% w.vec
    if(is.01){
      prod.vec <- sigmoid(-pred.vec * y.tilde)
      -t(X.int) %*% (y.tilde * prod.vec)
    } else {
      pred.vec - y.vec
    }
    
    #1/n ∑i=1^n L[w^T x_i + b, y_i] + penalty * ||w||_1
  }
  
  d.vec <- grad.loss(X.int, w.vec)
  
  u.vec <- w.vec + step.size * d.vec
  
  count <- 0
  max.loop <- 1000
  
  while((is.opt(d.vec, w.vec) == FALSE) & ( count < max.loop ) ){
    
    d.vec <- grad.loss(X.int, w.vec)
    
    u.vec <- w.vec + step.size * d.vec
    
    w.vec <- c(u.vec[1],soft(u.vec[-1], step.size * opt.thresh))
    count = count + 1
  }
  
  weight.vec <- w.vec
  return (weight.vec)
  
}

#' LinearModelL1penalties 
#' 
#' R function that calculates the L1 penalties
#' 
#' @param X.mat feature matrix (n_observations x n_features)
#' @param y.vec label vector (n_observations x 1)
#' @param penalty.vec non-negative numeric scalar
#' @param step.size numeric scalar for step size 
#' 
#' @return W.mat weight matrix on original scale (n_features+1 x n_penalties)
#' 
#' @export
#'
#' @examples
#' data(zip.train, package = "ElemStatLearn")
#' all.y.vec <- zip.train[,1]
#' is.01<- all.y.vec %in% c(0,1)
#' y.vec <- all.y.vec[is.01]
#' X.mat <- zip.train[is.01, -1]
#' n.folds <- 5
#' fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
#' validation.fold <- 1
#' is.train <- fold.vec != validation.fold
#' is.validation <- fold.vec == validation.fold
#' X.train <- X.mat[is.train,]
#' y.train <- y.vec[is.train]
#' step.size <- 0.1
#' penalty.vec <- c(1,2,3)
#' LinearModelL1penalties( X.train, y.train, penalty.vec, step.size )

LinearModelL1penalties <- function( X.mat, y.vec, penalty.vec, step.size ){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.numeric(y.vec)){
    stop("y.vec must be a vector")
  }
  if( !(is.numeric(penalty.vec)) ){
    stop("penalty.vec must be numeric vector")
  }
  if( !(is.numeric(step.size) && step.size > 0)) {
    stop("step.size must be numeric and greater than zero")
  }
  
  # scale X.mat
  X.scaled.mat <- scale(X.mat)
  
  # filter X.scaled.mat to remove 0 values
  # X.filtered <- X.scaled.mat[ , attr(X.scaled.mat, "scaled:scale") != 0]
  
  # intialize w.mat for storing w.vec for each penalty
  w.mat <- matrix(0, ncol(X.scaled.mat)+1, length(penalty.vec))
  
  # determine if y.vec is binary and create y.tilde if so
  is.01<- y.vec %in% c(0,1)
  
  # initialize opt.thresh and initial.weight.vec to prime loop
  opt.thresh <- 0.01
  initial.weight.vec <- rep( 0, l=ncol(X.scaled.mat)+1 )
  
  # loop through penalty.vec calling LinearModelL1 for each one
  index <- 1
  for( penalty in penalty.vec ) {
    w.tilde.opt.vec <- LinearModelL1( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size )
    
    # prime initial.weight.vec for warm restart
    initial.weight.vec <- w.tilde.opt.vec
    
    # unscale w.opt.sc.vec and 

    w.orig <- w.tilde.opt.vec[-1]/attr(X.scaled.mat, "scaled:scale")
    b.orig <- -t(w.tilde.opt.vec[-1]/attr(X.scaled.mat, "scaled:scale")) %*% attr(X.scaled.mat, "scaled:center")
    
    # store unscaled result
    w.mat[ , index ] <- c(b.orig, w.orig)
    index <- index + 1
  }
  
  return (w.mat)

}

#' LinearModelL1CV 
#' 
#' R function that calculates the L1 cross-validation
#' 
#' @param X.mat feature matrix (n_observations x n_features)
#' @param y.vec label vector (n_observations x 1)
#' @param fold.vec fold ID vector (n_observations x 1)
#' @param n.folds int scalar > 1
#' @param penalty.vec penalty vector (n_observations x 1)
#' @param step.size int scalar > 1
#' 
#' @return mean.validation.loss, mean.train.loss.vec, penalty.vec, selected.penalty, weight.vec, predict=function(testX.mat)
#' 
#' @export
#'
#' @examples
#' 
#' data(zip.train, package = "ElemStatLearn")
#' all.y.vec <- zip.train[,1]
#' is.01<- all.y.vec %in% c(0,1)
#' y.vec <- all.y.vec[is.01]
#' X.mat <- zip.train[is.01, -1]
#' n.folds <- 5
#' fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
#' step.size <- 0.1
#' penalty.vec <- c(1,2,3)


LinearModelL1CV <- function( X.mat, y.vec, fold.vec, n.folds=5, penalty.vec, step.size ){
  if(!is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  if(!is.numeric(y.vec)){
    stop("y.vec must be a vector")
  }
  if(!is.numeric(fold.vec)){
    stop("fold.vec must be a numeric vector")
  }
  if( !(is.numeric(n.folds) && n.folds > 0)) {
    stop("n.folds must be an integer and greater than zero")
  }
  if( !(is.numeric(penalty.vec)) ){
    stop("penalty.vec must be numeric vector")
  }
  if( !(is.numeric(step.size) && step.size > 0)) {
    stop("step.size must be numeric and greater than zero")
  }
  
  # should use K-fold cross-validation based on the fold IDs provided in fold.vec
  fold.ids <- unique(fold.vec)
  
  # determine if y.vec is binary to be used during loss calculation
  is.binary <- all(y.vec %in% c(0,1))
  
  # initalize loss matrix to store loss values for each fold
  fold.validation.loss.mat <- matrix(0, length(fold.ids), length(penalty.vec))
  fold.train.loss.mat <- matrix(0, length(fold.ids), length(penalty.vec))
  
  for(fold.i in fold.ids){
    is.validation <- which(fold.vec == fold.i)
    is.train <- which(fold.vec != fold.i)
    
    # For each train/validation split, use LinearModelL1penalties to compute the predictions for all observations
    fold.result <- LinearModelL1penalties( X.mat, y.vec, penalty.vec, step.size )
    fold.pred <- cbind(1, X.mat) %*% fold.result
    
    # Calculate the loss for the fold 
    # use the square loss for regression and the 01-loss for binary classification
    fold.loss <- if(is.binary){
      log(1+exp(-y.vec))
    }else{
      (fold.pred- y.vec)^2 / nrow(X.mat) 
    }
    
    fold.validation.loss <- fold.loss[is.validation]
    fold.train.loss <- fold.loss[is.train]

    # store fold loss in loss matrix
    fold.validation.loss.mat[fold.i, ] <- fold.validation.loss
    fold.train.loss.mat[fold.i, ] <- fold.train.loss
  }
  
  mean.validation.loss.vec <- colMeans(fold.validation.loss.mat)
  mean.train.loss.vec <- colMeans(fold.train.loss.mat)
  
  selected.penalty <- which.min(mean.validation.loss.vec)
  
  final.w.vec <- LinearModelL1penalties( X.mat, y.vec, selected.penalty, step.size )
  
  final.result <- list(mean.validation.loss <- mean.validation.loss,
                       mean.train.loss.vec <-mean.train.loss.vec,
                       penalty.vec <- penalty.vec,
                       selected.penalty <- selected.penalty,
                       weight.vec <- final.w.vec,
                       predict <- function(testX.mat){})
  
  return(final.result)
}
