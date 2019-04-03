data(ozone, package="ElemStatLearn")
head(ozone)
X.unscaled.mat <- as.matrix(ozone[,-1])
head(X.unscaled.mat)
X.scaled <- scale(X.unscaled.mat)
head(X.scaled.mat)
y.vec <- ozone[,1]
n.hidden.units <- 2 #u
set.seed(1) # for reproducibility if needed
V <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units)
w <- rnorm(n.hidden.units)

# loop
head( A <- X.scaled.mat %*% V) #1
sigmoid <- function(a){
  1/(1+exp(-a))
}
Z <- sigmoid(A) #2
head(b <- as.numeric(Z %*% w)) #3
head(delta.w <- b - y.vec) #4
head(A.deriv <- Z * (1-Z))
head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)) #5
head(grad.w <- t(Z) %*% delta.w /nrow(X.scaled.mat)) #6
head(grad.V <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)) #7

# take a step
step.size <- 0.1
w <- w - step.size * grad.w
V <- V - step.size * grad.V
sum(abs(c(grad.w, as.numeric(grad.V))))
# calc validation loss and stop when value goes up