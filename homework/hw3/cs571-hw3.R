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

# C 
ridgereg = function(X, Y, sigma.sq=1, tau.sq=1){
	lambda = sigma.sq / tau.sq 
	D = ncol(X)
	lambda.id = lambda * diag(D)
	print(lambda.id)
	X.prime.X = t(X) %*% X
	lambda.X.prime.X.inverse = solve(lambda.id + X.prime.X)
	X.prime.Y = t(X) %*% Y
	beta.hat = lambda.X.prime.X.inverse %*% X.prime.Y
	return(beta.hat)
}
beta.ridge = ridgereg(X, Y)
beta.ridge #=> 2.9972982, 0.7833412

# D 
# a
y.hat = X %*% beta.hat
RSSa = sum((Y - y.hat)^2)
RSSa #=> 181767.9
# b 
model.yhat = X %*% model$beta
RSSb = sum((Y - model.yhat)^2)
RSSb #=> 181767.9
# c
ridge.yhat = X %*% beta.ridge
RSSc = sum((Y - ridge.yhat)^2)
RSSc #=> 181768

# E
X.test = as.matrix(lindata[1001:1100 , 1:2])
Y.test = as.matrix(lindata[1001:1100 , 3])
# a
y.hat = X.test %*% beta.hat
RSSa = sum((Y.test - y.hat)^2)
RSSa #=> 16963.35
# b 
model.yhat = X.test %*% model$beta
RSSb = sum((Y.test - model.yhat)^2)
RSSb #=> 16963.35
# c
ridge.yhat = X.test %*% beta.ridge
RSSc = sum((Y.test - ridge.yhat)^2)
RSSc #=> 16963.59

# F 
X.hypo = matrix(c(1,135), nrow=1)
est1 = X.hypo %*% beta.hat
est1
est2 = X.hypo %*% model$beta
est2
est3 = X.hypo %*% beta.ridge
est3

# G 
X1 = as.matrix(lindata[1:1100 , 1])
X2 = as.matrix(lindata[1:1100, 2])
Y = as.matrix(lindata[1:1100 , 3])
plot(X1, Y)
plot(X2, Y)