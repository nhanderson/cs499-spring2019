% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l1regularization.R
\name{LinearModelL1penalties}
\alias{LinearModelL1penalties}
\title{LinearModelL1penalties}
\usage{
LinearModelL1penalties(X.mat, y.vec, penalty.vec, step.size)
}
\arguments{
\item{X.mat}{feature matrix (n_observations x n_features)}

\item{y.vec}{label vector (n_observations x 1)}

\item{penalty.vec}{non-negative numeric scalar}

\item{step.size}{numeric scalar for step size}
}
\value{
W.mat weight matrix on original scale (n_features+1 x n_penalties)
}
\description{
R function that calculates the L1 penalties
}
\examples{

}
