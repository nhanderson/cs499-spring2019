#' scaling input matix
Scaling <- function(X.mat){
  if( !is.matrix(X.mat)){
    stop("X.mat must be a matrix")
  }
  .colMeans(X.mat, m, n, na.rm = TRUE);
  scale( X.mat, center = m,scale = TURE );
  
}





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
  result<- .C("LMSquareLossIterations_interface", 
              as.integer(nrow(X.mat)),  #' n_train
              as.integer(nrow(X.mat)),  #' n_test
              as.integer(ncol(X.mat)),  #' n_features
              as.integer(max.interations),  #' max_iterations
              as.integer(step.size),   #' step_size
              as.double(X.mat),  #' train_input_ptr
              as.double(y.vec),  #' train_output_ptr
              weight_mat = double(ncol(X.mat) * max.iterations)  #' predictions_output_ptr
              )
  matrix(result.list$weight_mat, ncol(X.mat), max_iterations)
}


