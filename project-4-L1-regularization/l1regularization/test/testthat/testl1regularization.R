library(l1regularization)
library(testthat)
context("test L1 regularization")

data(zip.train, package = "ElemStatLearn")
all.y.vec <- zip.train[,1]
is.01<- all.y.vec %in% c(0,1)
y.vec <- all.y.vec[is.01]
X.mat <- zip.train[is.01, -1]
n.folds <- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
is.validation <- fold.vec == validation.fold
X.train <- X.mat[is.train,]
y.train <- y.vec[is.train]
X.scaled.mat <- scale(X.train)
X.filtered <- X.scaled.mat[, attr(X.scaled.mat, "scaled:scale") != 0]
opt.thresh <- 0.01
initial.weight.vec <- rep( 0, l=ncol(X.filtered) +1)
step.size <- 0.1
penalty <- 1


test_that("for valid inputs, LinearModelL1 output is of expected dimension", {
  weight.vec <- LinearModelL1( X.filtered, y.train, penalty, opt.thresh, initial.weight.vec, step.size )
  # dimenstion of weight vector is dependent on the amount of iterations the loop des
  expect_equal(array(weight.vec), dim(A) <- number )
  
})

test_that("for valid inputs, LinearModelL1penalties output is of expected dimension", {
  W.mat <- LinearModelL1penalties( X.mat, y.vec, penalty.vec, step.size )
  A <- c(ncol(X.filtered)+1, length(penalty.vec))
  expect_equal(dim(W.mat), dim(A))
  
})

test_that("for valid inputs, LinearModelL1CV output is of expected dimension", {
  final.result <- LinearModelL1CV(X.mat, y.vec, fold.vec, n.folds, penalty.vec, step.size  )
  expect_equal(length(final.result), 6)
  
})

test_that("for invalid inputs, LinearModelL1 function stops with informative message", {
  # should error on penalty !> 0
  try(LinearModelL1( X.filtered, y.train, penalty = as.integer(0), opt.thresh, initial.weight.vec, step.size ))
  
})

test_that("for invalid inputs, LinearModelL1penalties function stops with informative message", {
   # should error on X.train
  try(LinearModelL1penalties( X.train, y.vec, penalty.vec, step.size ))
  
})

test_that("for invalid inputs, LinearModelL1CV function stops with informative message", {
  # should error on n.fold not an integer
  try(LinearModelL1CV(X.mat, y.vec, fold.vec, n.folds = 0.1, penalty.vec, step.size  ))
  
})