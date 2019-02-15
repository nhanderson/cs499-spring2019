library(nearestNeighbors)
library(testthat)
context("nn")

test_that("nn computes same answer as R", {
  data(zip.train, package="ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[2]
  x <- zip.train[train.i, -1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  max.neighbors <- 3
  pred.vec <- nn(x, y, testx, max.neighbors)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  sorted.index.vec <- order(dist.vec)
  closest.indides <- sorted.index.vec[1:max.neighbors]
  cumsum(y[closest.indies])/(1:max.neighbors)
  expected.prediction <- sumsum(y[closoest.indices])/(1:max.neighbors)
  
  expect_equal(pred.vec, expected.prediction)
})

