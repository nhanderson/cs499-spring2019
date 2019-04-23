#' LinearModelL1 
#' 
#' 
#' 
#' @param X.scaled.mat
#' @param y.vec
#' @param penalty (non-negative numeric scalar)
#' @param opt.thresh (positive numeric scalar)
#' @param initial.weight.vec
#' @param step.size
#' 
#' @return optimal weight vector 
#'
#' @examples
#' 
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
  if( !(is.numeric(opt.threash) && penalty > 0) ){
    stop("penalty must be numeric and greater than zero")
  }
  if( !is.numeric(initial.weight.vec)){
    stop("initial.weight.vec must be a vector")
  }
  if( !(is.numeric(step.size) && step.size > 0)) {
    stop("step.size must be numeric and greater than zero")
  }
  
  
}

#' LinearModelL1penalties 
#' 
#' 
#' 
#' @param 
#' 
#' @return 
#'
#' @examples
#' 


#' LinearModelL1CV 
#' 
#' 
#' 
#' @param 
#' 
#' @return 
#'
#' @examples

