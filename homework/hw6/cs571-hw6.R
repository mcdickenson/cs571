# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 6

setwd('~/github/cs571/homework/hw6')
library(MCMCpack) # for Dirichlet distribution


# Problem 1
X = read.table("HW6_mixture.txt", as.is=TRUE, header=FALSE)
X = as.matrix(X)
dim(X)
head(X)

# set hyperparameters
# M = 1e4
M = 10
MIN = 0
MAX = 5
K = 2

# initialize
pis = mus = matrix(NA, nrow=M, ncol=K)
pis[1, ] = rep(1/K, K)
mus[1, ] = runif(K, MIN, MAX)
alpha = rep(1, K)
Z <- matrix(NA, nrow=nrow(X), ncol=K)
Z[1,] <- alpha/K
S_0 = rep(1, K)
v_0 = rep(1, K)
v = rep(0, K)
S = rep(0, K)
Sigma.inv = rgamma(K, rate=S_0, shape=v_0)
x.bar = rep(0, K)
N = rep(0, K)
z = rep(0, nrow(X))

for(m in 2:M){
	# step 1 - simulate proportion vector from Dirichlet
	pisamp = rdirichlet(1, c(alpha[1]+N[1], alpha[2]+N[2]) )
	pis[m, ] = pisamp[1, ]
	
	# step 2 - sample latent indicators
	for(i in 1:nrow(Z)){
		for(j in 1:ncol(Z)){
			Z[i, j] = pis[m, j] * dnorm(X[i], mus[m-1, j], Sigma.inv[j])
		}
		Z[i,] = Z[i,]/sum(Z[i,])

		# step 3 - calculate new N_k's based on Z draws
		z[i] = sample(seq(1:K), size=1, prob=Z[i,])
	}

	# step 4 - update mean and variance
	for(k in 1:K){
		want = which(z==k)
		N[k] = length(want)
		subset = X[want]
		x.bar[k] = mean(subset)
		v[k] = v_0[k] + N[k]
		S[k] = S_0[k] + sum.sq.err(subset, mus[m-1,k]) # test this
		Sigma.inv[k] = rgamma(1, shape=v[k], rate=S[k])

		lower = max(c(MIN, mus[m-1, k]-1))
		upper = min(c(MAX, mus[m-1, k]+1))

		mus[m, k] = runif(1, lower, upper)
	}
	print(m)
}


sum.sq.err = function(x, mu){
	errs = (x-mu) 
	sq.errs = errs %*% t(errs)
	sse = sum(sq.errs)
	return(sse)
}

