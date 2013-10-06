# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 4

# Problem 3

setwd('~/github/cs571/homework/hw4')
X = read.table("HW4_problem3.txt", header=FALSE)
dim(X)
head(X)
X[1:10, 1]


mykmeans = function(x, maxiters=100){
	# step 1
	centroids = eta_0 = c(mean(x)) #eta_0
	# print(centroids)
	k = 1
	iters = 0 
	clusters = matrix(1, nrow=length(x), ncol=1)

	converged = FALSE

	while(!converged){
		# step 2
		# new_eta = sample(x, 1)
		# last_eta = new_eta 
		new_eta = rnorm(1, eta_0, sd(x))
		centroids = c(centroids, new_eta)

		# step 3
		new_clusters = cluster(x=x, centroids=centroids)
		# print(new_clusters)
		# if(new_clusters==clusters[ , ncol(clusters)]){ converged=TRUE}
		clusters = cbind(clusters, new_clusters)
		# print(clusters)

		# step 4
		# print("new_clusters")
		# print(new_clusters)
		print("len(centroids)")
		print(length(centroids))
		to_keep = which(c(1:length(centroids)) %in% new_clusters)
		
		print("to_keep")
		print(to_keep)
		keep = centroids[to_keep]

		# print(keep)
		print("centroids before keep")
		print(centroids)

		lastk = k
		# print(lastk)
		k = length(keep)
		# print(k)
		centroids = keep
		print("centroids to keep")
		print(centroids)
		# update retained centroids
		for(i in 1:length(centroids)){
			subset = x[which(new_clusters[1, ]==i)]
			centroids[i] = mean(subset)
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
	# print(n)
	# print(k)
	labels = matrix(NA, nrow=n, ncol=1)

	for(i in 1:n){
		# print(i)
		dists = matrix(NA, nrow=1, ncol=k)
		for(j in 1:k){
			# print(j)
			dists[ , j] = euclid(x[i], centroids[j])
		}
		# print("dists:")
		# print(dists)
		# print(min(dists))

		# print("centroids")
		# print(centroids)
		labels[i, ] = which(dists == min(dists, na.rm=TRUE))
	}
	return(labels)
}

euclid = function(a, b){
	# print("a:") 
	# print(a)
	# print("b:") 
	# print(b)
	return((a-b)^2)
}

answer3 = mykmeans(X[1:1000,1])
answer3

X[1:10, ]

# kmeans(X[1:1000,1], centers=7)$centers


head(X)

mean(X)

?kmeans
?min


# 		 [990,]    3
#  [991,]    3
#  [992,]    1
#  [993,]    3
#  [994,]    3
#  [995,]    3
#  [996,]    3
#  [997,]    3
#  [998,]    1
#  [999,]    1
# [1000,]    1
# [1] "len(centroids)"
# [1] 3
# [1] "to_keep"
# [1] 1 3
# [1] "centroids before keep"
# [1] 179.0773      NaN 164.1851
# [1] "centroids to keep"
# [1] 179.0773 164.1851
# > answer3
# $K
# [1] 2

# $numiters
# [1] 2

# $means
# [1] 179.0773      NaN