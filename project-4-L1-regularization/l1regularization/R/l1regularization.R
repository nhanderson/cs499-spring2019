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

  #initialize w w/ intercept col, first term is bias/intercept
  w <- rep( 0, l=ncol(X.scaled.mat)+1 )
  
  sigmoid <- function(z){
    1/(1+exp(-z))
  }
  
  sign <- function(x){
    if(x > 0){ 1 } 
    else if (x < 0) { -1 } 
    else { 0 }
  }
  
  posPart <- function(x) {
    if( x > 0 ) { x } 
    else { 0 }
  }
  
  soft <- function(x,lambda){
    sign(x) * postPart((abs(x-lambda)))
  }
  is.01 <- all(y.vec %in% c(0,1))
  
  if(is.01){
    y.tilde <- ifelse(y.vec==1,1,-1)
  }
  
  grad.loss <- function(w){
    X.int <- cbind(1, X.scaled.mat)
    pred.vec <- X.int %*% w
    
    if(is.01){
    prod.vec <- sigmoid(-pred.vec * y.tilde)
    -t(X.int) %*% (y.tilde * prod.vec)
    }
    else{
      pred.vec - y.vec
    }
  }
  
  d.vec <- -grad.loss(w.vec)

  u.vec <- w.vec + step.size * d.vec

  weight.vec <- c(u.vec[1],soft(u.vec[-1], step.size * opt.thresh))
  
  return (weight.vec)
  
}

#' LinearModelL1penalties 
#' 
#' 
#' 
#' @param X.mat
#' @param y.vec
#' @param penalty.vec
#' @param step.size
#' 
#' @return weight matrix on original scale 
#'
#' @examples
#' 
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
  
  X.scaled.mat <- scale(X.mat)
  
  # is.01 <- all.y.vec %in% c(0,1)
  
  #if(is.01){
   # y.tilde <- ifelse(y.vec==1,1,-1)
  #}
  
  X.filtered <- X.scaled.mat[ , attr(X.sc, "scaled:scale") != 0]
  
  initial.weight.vec <- penalty.vec[,1]
  
  for(n in c(1:ncol(penalty.vec))){
    weight.vec <- LinearModelL1(X.filtered, y.vec, penalty.vec, opt.thresh, initial.weight.vec, step.size)
    initial.weight.vec <- weight.vec
    weight.vec.orig <- weight.vec/attr(attr(X.scaled.mat, "scaled:scale"))
    W.mat <- rbind(weight.vec.orig)
  }
  
  return (W.mat)
  
}

#' LinearModelL1CV 
#' 
#' 
#' 
#' @param X.mat
#' @param y.vec
#' @param fold.vec
#' @param n.folds
#' @param penalty.vec
#' @param step.size
#' 
#' @return 
#'
#' @examples
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
  if( !(is.integer(n.folds) && n.folds > 0)) {
    stop("n.folds must be an integer and greater than zero")
  }
  if( !(is.numeric(penalty.vec)) ){
    stop("penalty.vec must be numeric vector")
  }
  if( !(is.numeric(step.size) && step.size > 0)) {
    stop("step.size must be numeric and greater than zero")
  }
  
  set.seed(1)

}
