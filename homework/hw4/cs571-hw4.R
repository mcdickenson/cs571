# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 4

# Problem 3

setwd('~/github/cs571/homework/hw4')
X = read.table("HW4_problem3.txt", header=FALSE)
dim(X)
head(X)

mykmeans = function(x, maxiters=100){
	# step 1
	centroids = eta_0 = c(mean(x)) #eta_0
	k = 1
	iters = 0 
	clusters = matrix(1, nrow=length(x), ncol=1)

	converged = FALSE

	while(!converged){
		# step 2
		last_eta = centroids[length(centroids)]
		new_eta = rnorm(1, eta_0, sd(x))
		centroids = c(centroids, new_eta)

		# step 3
		new_clusters = cluster(x=x, centroids=centroids)
		clusters = cbind(clusters, new_clusters)

		# step 4
		to_keep = which(c(1:length(centroids)) %in% new_clusters)

		keep = centroids[to_keep]

		lastk = k
		k = length(keep)

		centroids = keep

		# update retained centroids
		for(i in 1:length(centroids)){
			subset = x[which(new_clusters[, 1]==i)]
			centroids[i] = mean(subset, na.rm=TRUE)
		}

		iters = iters + 1
		if(k==lastk || iters>=maxiters){ converged=TRUE }
	}

	output = list(K=k, numiters=iters, means=centroids)
	return(output)
}

cluster = function(x, centroids){
	# assign each X_i to one of k+1 clusters
	n = length(x)
	k = length(centroids)
	labels = matrix(NA, nrow=n, ncol=1)

	for(i in 1:n){
		dists = matrix(NA, nrow=1, ncol=k)
		for(j in 1:k){
			dists[ , j] = euclid(x[i], centroids[j])
		}
		labels[i, ] = which(dists == min(dists, na.rm=TRUE))
	}
	return(labels)
}

euclid = function(a, b){
	return((a-b)^2)
}

answer3 = mykmeans(X[1:1000,1])
answer3

mykmeans2 = function(x, maxiters=100){
	# step 1
	centroids = eta_0 = c(mean(x)) #eta_0
	k = 1
	iters = 0 
	clusters = matrix(1, nrow=length(x), ncol=1)

	converged = FALSE

	while(!converged){
		# step 2
		last_eta = centroids[length(centroids)]
		new_eta = rnorm(1, eta_0, sd(x))
		centroids = c(centroids, new_eta)

		# step 3
		new_clusters = cluster(x=x, centroids=centroids)
		clusters = cbind(clusters, new_clusters)

		# step 4 
		to_keep = c()
		for(i in 1:length(centroids)){
			occurs = length(which(new_clusters==i))
			if(occurs>=10){ to_keep = c(to_keep, i)}
		}
		# which(c(1:length(centroids)) %in% new_clusters)

		keep = centroids[to_keep]

		lastk = k
		k = length(keep)

		centroids = keep

		# update retained centroids
		for(i in 1:length(centroids)){
			subset = x[which(new_clusters[, 1]==i)]
			centroids[i] = mean(subset, na.rm=TRUE)
		}

		iters = iters + 1
		if(k==lastk || iters>=maxiters){ converged=TRUE }
	}

	output = list(K=k, numiters=iters, means=centroids)
	return(output)
}

answer3c = mykmeans2(X[1:1000,1])
answer3c
