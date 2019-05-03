library(l1regularization)
library(testthat)
context("test L1 regularization")

data(zip.train, package="ElemStatLearn")
y.vec <- all.y.vec[is.01]
X.mat <- zip.train[is.01, -1]

set.seed(1)
n.folds <- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))

X.sc <- scale(X.train)


test_that("for valid inputs, LinearModelL1 output is of expected dimension", {
  LinearModelL1( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size )
  
  
})

test_that("for valid inputs, LinearModelL1penalties output is of expected dimension", {
  
  
})

test_that("for valid inputs, LinearModelL1CV output is of expected dimension", {
  LinearModelL1CV(X.mat, y.vec, fold.vec, n.folds, penalty.vec, step.size  )
  
})

test_that("for invalid inputs, LinearModelL1 function stops with informative message", {
  
  
})

test_that("for invalid inputs, LinearModelL1penalties function stops with informative message", {
  
  
})

test_that("for invalid inputs, LinearModelL1CV function stops with informative message", {
  
  
})