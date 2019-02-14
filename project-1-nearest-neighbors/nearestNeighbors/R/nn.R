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
  result.list <- .c("NN1toKmaxPredict_interface", as.double(x.mat), as.double(y.vec), as.double(testx.vec), 
                    as.integer(nrow(x.mat)), as.integer(ncol(x.mat)), as.integer(max.neighbors), 
                    predictions-double(max.neighbors), PACKAGE="nearestNeighbor")
  result.list$predictions
}
