# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 6

setwd('~/github/cs571/homework/hw6')
library(MCMCpack) # for Dirichlet distribution


# Problem 1
X = read.table("HW6_mixture.txt", as.is=TRUE, header=FALSE)
X = as.matrix(X)
plot(density(X))
min(X)
max(X)
dim(X)
head(X)

# helper functions
sum.sq.err = function(x, mu){
	errs = (x-mu) 
	sq.errs = errs %*% t(errs)
	sse = sum(sq.errs)
	return(sse)
}

log.lik = function(x, z, mu){
	ll = 0 
	for(i in 1:nrow(X)){
		# print(x)
		mu.tmp = mu[z[i]]
		tmp = log(dnorm(x[i], mu.tmp, 1))
		ll = ll + tmp 
	}
	return(ll)
}

# settings 
BURN = 500
M = 3000+BURN
STEP = 0.1
MIN = 0
MAX = 5
K = 2

# initialize
pis = mus = matrix(NA, nrow=M, ncol=K)
mus[1, ] = sample(X, 2)
mus[1,]
Z = matrix(NA, nrow=nrow(X), ncol=K)
Z[1,] = pis[1, ] = rep(1/K, K)
N = x.bar = rep(0, K)
z = rep(0, nrow(X))
alpha = rep(1, K)
S_0 = rep(3, K) 
v_0 = rep(5, K) 
v = rep(0, K)   
S = rep(0, K)   
Sigma.inv = rgamma(K, shape=v_0, rate=S_0)
new.mu = rep(NA, K)
likelihood.trace =  matrix(NA, nrow=M, ncol=1)


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

		lower = max(c(MIN, mus[m-1, k]-STEP))
		upper = min(c(MAX, mus[m-1, k]+STEP))
		new.mu[k] = runif(1, lower, upper)

	}

	ll.new.mu = log.lik(X, z, new.mu)
	ll.old.mu = log.lik(X, z, mus[m-1,])

	acceptance.prob = ll.new.mu / ll.old.mu 
	acceptance.prob = min(1, acceptance.prob, na.rm=TRUE)
	if(runif(1,0,1)<acceptance.prob){
		mus[m, ] = new.mu 
		likelihood.trace[m, 1] = ll.new.mu
	} else {
		mus[m, ] = mus[m-1, ]
		likelihood.trace[m, 1] = ll.old.mu
	}
		
	if(m%%10==0){ print(m) }
}


plot(1:M, mus[, 1], type='l')
plot(1:M, mus[, 2], type='l')
plot(1:M, likelihood.trace, type='l')

mean(mus[(BURN+1):M, 1])
mean(mus[(BURN+1):M, 2])

length(which(z==1))
length(which(z==2))


# save.image("success-1.RData") # ok
# save.image("success-2.RData") # beautiful, except likelihood trace
# save.image("success-3.RData") # pretty good



# D: Show the log likelihood trace for three different runs of the sampler starting at three different points on the data you downloaded.

# posterior samples post burn-in

# load("success-1.RData")
# load("success-2.RData")
# load("success-3.RData")

# pdf("mu1.pdf")
# histogram(mus[(BURN+1):M, 1], xlab="mu1")
# # histogram(mus[(BURN+1):M, 2], xlab="mu2")
# dev.off()

# pdf("mu2.pdf")
# # histogram(mus[(BURN+1):M, 1], xlab="mu1")
# histogram(mus[(BURN+1):M, 2], xlab="mu2")
# dev.off()

