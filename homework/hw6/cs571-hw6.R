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

# helper functions
sum.sq.err = function(x, mu){
	errs = (x-mu) 
	sq.errs = errs %*% t(errs)
	sse = sum(sq.errs)
	return(sse)
}

log.lik = function(x, mu){
	ll = sum(log(dnorm(x, mu,1)))
	return(ll)
}

# initialize
pis = mus = matrix(NA, nrow=M, ncol=K)
mus[1, ] = runif(K, MIN, MAX)
Z = matrix(NA, nrow=nrow(X), ncol=K)
Z[1,] = pis[1, ] = rep(1/K, K)
N = x.bar = rep(0, K)
z = rep(0, nrow(X))
alpha = rep(1, K)
S_0 = rep(5, K) # set this
v_0 = rep(5, K) # set this
v = rep(0, K)   
S = rep(0, K)   
Sigma.inv = rgamma(K, rate=S_0, shape=v_0)


for(m in 2:M){
	# step 1 - simulate proportion vector from Dirichlet
	pisamp = rdirichlet(1, c(alpha[1]+N[1], alpha[2]+N[2]) )
	pis[m, ] = pisamp[1, ]
	
	# step 2 - sample latent indicators
	for(i in 1:nrow(Z)){
		for(j in 1:ncol(Z)){
			Z[i, j] = pis[m, j] * dnorm(X[i], mus[m-1, j], 1/Sigma.inv[j])
		}
		Z[i,] = Z[i,]/sum(Z[i,])

		# step 3 - calculate new z's based on Z draws
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
		new.mu = runif(1, lower, upper)
		ll.new.mu = log.lik(subset, new.mu)
		ll.old.mu = log.lik(subset, mus[m-1,k])
		acceptance.prob = ll.new.mu / ll.old.mu 
		acceptance.prob = min(1, acceptance.prob)
		if(runif(1,0,1)<acceptance.prob){
			mus[m, k] = new.mu 
		} else {
			mus[m, k] = mus[m-1, k]
		}
		# print(acceptance.prob)

		
	}
	print(m)
}



# How many iterations of Burn-In did you run? 
# How many iterations of sampling did you run? 
# How did you initialize your parameters?
# Show the log likelihood trace for three different runs of the sampler starting at three different points on the data you downloaded.
# Plot a histogram of the posterior samples for each mean parameter for a single run (after burn-in)

x = c(-3, -2, -1, 1,2,3)

