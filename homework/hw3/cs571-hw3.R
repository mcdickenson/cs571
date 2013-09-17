# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 3

setwd('~/github/cs571/homework/hw3')
lindata = read.table("HW3_linear_regression.txt", header=TRUE)
dim(lindata)
head(lindata)

logdata = read.table("HW3_logistic_regression.txt", header=TRUE)
dim(logdata)
head(logdata)

# Problem 1
# A 
X = as.matrix(lindata[1:1000 , 1:2])
Y = as.matrix(lindata[1:1000 , 3])
X.prime.X = t(X) %*% X
X.prime.X.inverse = solve(X.prime.X)
X.prime.Y = t(X) %*% Y
beta.hat = X.prime.X.inverse %*% X.prime.Y
beta.hat #=> 3.0117879, 0.7832938

# B
linreg = function(X, Y){
	RSS = function(b, x, y){
		residuals = y - (x %*% b )
		sq.resid = residuals^2
		rss = sum(sq.resid)
		return(rss)
	}
	results = optim(rep(0, ncol(X)), RSS,
		hessian=TRUE, method="BFGS", x=X, y=Y)
	list(beta=results$par, 
		vcov=solve(results$hessian),
		converged=results$convergence==0)
}
model = linreg(X, Y)
model$beta #=> 3.0117879, 0.7832938


