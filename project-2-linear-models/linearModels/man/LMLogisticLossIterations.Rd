% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EarlyStopping.R
\name{LMLogisticLossIterations}
\alias{LMLogisticLossIterations}
\title{LMLogisticLossIterations function}
\usage{
LMLogisticLossIterations(X.mat, y.vec, max.iterations, step.size)

LMLogisticLossIterations(X.mat, y.vec, max.iterations, step.size)
}
\arguments{
\item{X.mat}{numeric train featrue matrix [n x p]
R function that}

\item{y.vec}{numeric train label vector [n]}

\item{step.size}{scalar integer, step size}

\item{X.mat}{numeric train feature matrix [n x p]}

\item{max.iteratons}{scalar integer greater than 1, max number of iterations}
}
\value{
matrix of weight vectors
}
\description{
R function that wraps the C++ code.
}
\examples{
data(spam, package="ElemStatLearn")
x <- spam[1, ]
y <- spam[, 'spam']
LMSquareLossIterations(x ,y, as.integer(12), as.integer(0.5))

}
