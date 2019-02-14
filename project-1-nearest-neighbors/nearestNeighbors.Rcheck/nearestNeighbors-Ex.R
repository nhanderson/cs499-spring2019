pkgname <- "nearestNeighbors"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('nearestNeighbors')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("nn")
### * nn

flush(stderr()); flush(stdout())

### Name: nn
### Title: nearest neighbors algorithm
### Aliases: nn

### ** Examples

data(zip.train, package-"ElemStatLearn")
i01 <- which(zip.train[,1] %in% c(0,1))
train.i <- i01[1:5]
test.i <- i01[6]
x <- zip.train[train.i, -1]
y <- zip.train[train.i, 1]
testx <- zip.train[test.i, -1]
nn(x ,y, testx , 3)
zip.train[test.i, 1]




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
