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

normaleqn = function(x, y){
	x.prime.x.inverse = solve(t(x) %*% x)
	x.prime.y = t(x) %*% y 
	beta = x.prime.x.inverse %*% x.prime.y
	return(beta)
}

beta.hat = normaleqn(X, Y)
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
X1 = as.matrix(lindata[1:1000 , 1])
X2 = as.matrix(lindata[1:1000, 2])
Y = as.matrix(lindata[1:1000 , 3])
summary(X1)
summary(X2)
summary(Y)

# x1 plots

# (Y - ols_beta_2 * X2 - ols_beta_0) vs X1

# normal eqn
pdf("p1x1-normal.pdf")
plot(X1, (Y-beta.hat[3,]*X2-beta.hat[1,]),
	main="Normal Eqn")
# lines(X1, beta.hat[2,]*X1)
abline(0, beta.hat[2,], col="blue")
dev.off()

# sgd
pdf("p1x1-sgd.pdf")
plot(X1, (Y-model$beta[3]*X2-model$beta[1]), 
	main="SGD")
# lines(X1, model$beta[2]*X1)
abline(0, model$beta[2], col="blue")
dev.off()

# ridge
pdf("p1x1-ridge.pdf")
plot(X1, (Y-beta.ridge[3,]*X2-beta.ridge[1,]), 
	main="Ridge")
# lines(X1, beta.ridge[2,]*X1)
abline(0, beta.ridge[2,], col="blue")
dev.off()

# x2 plots

# normal
pdf("p1x2-normal.pdf")
plot(X2, (Y-beta.hat[2,]*X1-beta.hat[1,]),
	main="Normal Eqn")
abline(0, beta.hat[3,], col="blue")
dev.off()

# sgd
pdf("p1x2-sgd.pdf")
plot(X2, (Y-model$beta[2]*X1-model$beta[1]),
	main="SGD")
abline(0, model$beta[3], col="blue")
dev.off()

# ridge
pdf("p1x2-ridge.pdf")
plot(X2, (Y-beta.ridge[2,]*X1-beta.ridge[1,]),
	main="Ridge")
abline(0, beta.ridge[3,], col="blue")
dev.off()





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

w = irls(X, Z) #=> -13.52371957, 0.24204760, 0.08109647
w

# B 
beta = w[, 1]
z.hat =sigm(X %*% beta)
head(z.hat)
summary(z.hat)
RSSa = sum((Z - z.hat)^2)
RSSa #=> 141.4738

# C 
X.test = as.matrix(logdata[1001:1100 , 1:2])
X.test = cbind(1, X.test)
Z.test = as.matrix(logdata[1001:1100 , 3])
z.test.hat = sigm(X.test %*% beta)
summary(z.test.hat)
RSSb = sum((Z.test - z.test.hat)^2)
RSSb #=> 15.73171

# D
X.hypo = matrix(c(1,1,135), ncol=1)
Z.hypo = sigm(beta %*% X.hypo)
Z.hypo

# E
X1 = as.matrix(logdata[1:1000 , 1])
X2 = as.matrix(logdata[1:1000 , 2])
Z = as.matrix(logdata[1:1000 , 3])

beta

pdf("p2x1.pdf")
plot(X1, Z-beta[3]*X2-beta[1])
abline(0, beta[2], col="blue")
dev.off()

pdf("p2x2.pdf")
plot(X2, Z-beta[2]*X1-beta[1])
abline(0, beta[3], col="blue")
dev.off()

# pdf("p2x1.pdf")
# plot(jitter(X1, amount=0.05), jitter(Z, amount=0.05),
# 	xlab="X1", ylab="Z", ylim=c(-14,1))
# # abline(sigm(beta[1]), sigm(beta[2]))
# abline(beta[1], beta[2], col="blue", lwd=2)
# dev.off()

# pdf("p2x2.pdf")
# plot(X2, jitter(Z, amount=0.05),
# 	xlab="X2", ylab="Z")
# abline(beta[1], beta[3], col="blue", lwd=2)
# dev.off()


# F 
cor(logdata[, 1], logdata[, 2])