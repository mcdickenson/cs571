# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 3

# Problem 1

setwd('~/github/cs571/homework/hw3')
lindata = read.table("HW3_linear_regression.txt", header=TRUE)
dim(lindata)
head(lindata)

# A 
X = as.matrix(lindata[1:1000 , 1:2])
X = cbind(1, X)
Y = as.matrix(lindata[1:1000 , 3])
X.prime.X = t(X) %*% X
X.prime.X.inverse = solve(X.prime.X)
X.prime.Y = t(X) %*% Y
beta.hat = X.prime.X.inverse %*% X.prime.Y
beta.hat #=> 69.942167, 15.119109, 0.321028

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
model$beta #=> 69.942167, 15.119109, 0.321028

# C 
ridgereg = function(X, Y, sigma.sq=1, tau.sq=1){
	lambda = sigma.sq / tau.sq 
	D = ncol(X)
	lambda.id = lambda * diag(D)
	X.prime.X = t(X) %*% X
	lambda.X.prime.X.inverse = solve(lambda.id + X.prime.X)
	X.prime.Y = t(X) %*% Y
	beta.hat = lambda.X.prime.X.inverse %*% X.prime.Y
	return(beta.hat)
}
beta.ridge = ridgereg(X, Y)
beta.ridge #=> 65.9129285, 14.3521921, 0.3478858

# D 
# a
y.hat = X %*% beta.hat
RSSa = sum((Y - y.hat)^2)
RSSa #=> 98729.33
# b 
model.yhat = X %*% model$beta
RSSb = sum((Y - model.yhat)^2)
RSSb #=> 98729.33
# c
ridge.yhat = X %*% beta.ridge
RSSc = sum((Y - ridge.yhat)^2)
RSSc #=> 99005.91

# E
X.test = as.matrix(lindata[1001:1100 , 1:2])
X.test = cbind(1, X.test)
Y.test = as.matrix(lindata[1001:1100 , 3])
# a
y.hat = X.test %*% beta.hat
RSSa = sum((Y.test - y.hat)^2)
RSSa #=> 11573.24
# b 
model.yhat = X.test %*% model$beta
RSSb = sum((Y.test - model.yhat)^2)
RSSb #=> 11573.24
# c
ridge.yhat = X.test %*% beta.ridge
RSSc = sum((Y.test - ridge.yhat)^2)
RSSc #=> 11523.47

# F 
X.hypo = matrix(c(1,1,135), nrow=1)
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






# Problem 2

logdata = read.table("HW3_logistic_regression.txt", header=TRUE)
dim(logdata)
head(logdata)

# A 
X = as.matrix(logdata[1:1000 , 1:2])
X = cbind(1, X)
Z = as.matrix(logdata[1:1000 , 3])

sigm = function(eta){
	out = exp(eta)/( exp(eta) + 1)
	return(out)
}

irls = function(X, Y, epsilon=1/1e10){
	D = ncol(X)
	N = nrow(X)
	
	w = matrix(0, nrow=D, ncol=D) 
	w0 = log(mean(Y)/(1-mean(Y)))
	eta = mu = s = z = matrix(NA, nrow=N, ncol=D)
	
	converged = FALSE
	numiters = 0 
	while(!converged){
		last.w = w 
		for(i in 1:N){
			xi = matrix(X[i, ], ncol=1) 
			eta[i, ] = w0 + (t(w) %*% xi)
			mu[i, ] = sigm(eta[i, ])
			s[i, ] = mu[i, ] * (1-mu[i, ])
			z[i, ] = eta[i, ] + ((Y[i, ] - mu[i, ])/s[i, ])
		}
		S = matrix(0, nrow=N, ncol=N)
		for(i in 1:N){
			S[i, i] = s[i]
		}
		first.term = solve(t(X) %*% S %*% X)
		second.term = t(X) %*% S %*% z 
		w =  first.term %*% second.term
		numiters = numiters + 1
		diff = w - last.w 
		smalldiff = TRUE
		for(i in 1:D){
			smalldiff = smalldiff & (diff[i, i] < epsilon)
		}
		if(smalldiff){ 
			converged = TRUE 
			output = paste("converged after ", numiters, " iterations", sep="")
			print(output)
		}
		if(numiters > 100){
			print("reached max iterations")
			break 
		}
	}
	return(w)
}

irls(X, Z)
