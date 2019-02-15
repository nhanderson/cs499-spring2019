library(nearestNeighbors)
library(test_that)

context("nn")

test_that("nn computes same answer as R", {
  data(zip.train, package-"ElemStatLearn")
  i01 <- which(zip.train[,1] %in% c(0,1))
  train.i <- i01[1:5]
  test.i <- i01[6]
  x <- zip.train[train.i, -1]
  y <- zip.train[train.i, 1]
  testx <- zip.train[test.i, -1]
  nn(x ,y, testx , 3)
  zip.train[test.i, 1]
})