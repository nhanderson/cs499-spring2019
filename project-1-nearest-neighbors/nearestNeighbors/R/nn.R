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
#' data(zip.train, package-"ElemStatLearn")
#' i01 <- which(zip.train[,1] %in% c(0,1))
#' train.i <- i01[1:5]
#' test.i <- i01[6]
#' x <- zip.train[train.i, -1]
#' y <- zip.train[train.i, 1]
#' testx <- zip.train[test.i, -1]
#' nn(x ,y, testx , 3)
#' zip.train[test.i, 1]
#' 
nn <- function(x.mat, y.vec, testx.vec, max.neighbors){
  result.list <- .C("NN1toKmaxPredict_interface", 
                    as.integer(nrow(x.mat)), #' n_train_observations
                    as.integer(nrow(x.mat)), #' n_test_observations
                    as.integer(ncol(x.mat)), #' n_features
                    as.integer(max.neighbors), #' max_neighbors
                    as.double(x.mat), #' train_in_ptr
                    as.double(y.vec), #' train_out_ptr
                    as.double(testx.vec), #' test_in_ptr
                    integer(max.neighbors), #' predictions_out_ptr
                    PACKAGE="nearestNeighbor")
  result.list$predictions
}

