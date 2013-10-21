# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 5

setwd('~/github/cs571/homework/hw5')

# Problem 1
# a
set.seed(123)
n = p = 100
k = 3
phi = c(0.2, 2, 10)
z = matrix(rnorm(n*k, 0, 1), nrow=k, ncol=n)
L = matrix(rnorm(p*k, 0, 1), nrow=p, ncol=k)
epsilon1 = matrix(rnorm(p*n, 0, phi[1]), nrow=p, ncol=n)
epsilon2 = matrix(rnorm(p*n, 0, phi[2]), nrow=p, ncol=n)
epsilon3 = matrix(rnorm(p*n, 0, phi[3]), nrow=p, ncol=n)
X1 = L%*%z + epsilon1
X2 = L%*%z + epsilon2
X3 = L%*%z + epsilon3

eigen1 = eigen(cov(X1))$values
eigen2 = eigen(cov(X2))$values
eigen3 = eigen(cov(X3))$values

pdf("1a.pdf")
par(mfrow=c(1,3))
plot(density(eigen1), main="X1")
plot(density(eigen2), main="X2")
plot(density(eigen3), main="X3")
dev.off()

# b
RMSE = function(a, b){
	sq.err = (a - b)^2
	mean.sq.err = mean(sq.err)
	root.mean.sq.err = sqrt(mean.sq.err)
	return(root.mean.sq.err)
}

x1eigvec = eigen(cov(X1))$vectors[, 1:3]
x1eigval = diag(eigen1[1:3])
recon1 = x1eigvec %*% x1eigval %*% t(x1eigvec)
rmse1 = RMSE(cov(X1), recon1)
rmse1 #=> 0.005441325

x2eigvec = eigen(cov(X2))$vectors[, 1:3]
x2eigval = diag(eigen2[1:3])
recon2 = x2eigvec %*% x2eigval %*% t(x2eigvec)
rmse2 = RMSE(cov(X2), recon2)
rmse2 #=> 0.5532188

x3eigvec = eigen(cov(X3))$vectors[, 1:3]
x3eigval = diag(eigen3[1:3])
recon3 = x3eigvec %*% x3eigval %*% t(x3eigvec)
rmse3 = RMSE(cov(X3), recon3)
rmse3 #=> 13.02954


# Problem 2
train = read.table("training_data_gp.txt", as.is=TRUE, header=FALSE)
dim(train)
n = nrow(train)
test = read.table("test_data_gp.txt", as.is=TRUE, header=FALSE)
dim(test)
names(train) = names(test) = c("sequence", "intensity")

letterCount = function(letter, string){
	count = length(which(unlist(strsplit(string,NULL))==letter))
	return(count)
}

train$A = train$C = train$G = train$T = NA 
for(i in 1:n){
	train$A[i] = letterCount("A", train$sequence[i])
	train$C[i] = letterCount("C", train$sequence[i])
	train$G[i] = letterCount("G", train$sequence[i])
	train$T[i] = letterCount("T", train$sequence[i])
}

kernel = function(x1, x2){
	dist = (x1$A - x2$A)^2 + (x1$C - x2$C)^2 + (x1$G - x2$G)^2 + (x1$T - x2$T)^2 
}

gram = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in 1:n){
		gram[i,j] = kernel(train[i, ], train[j, ])
	}
	print(i)
}
save(gram, file="gram.rda")


