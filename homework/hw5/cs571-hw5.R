# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 5

setwd('~/github/cs571/homework/hw5')
require(utils)
require(stringr)

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



# a 
combos1 = c("A", "C", "G", "T")
substrings = expand.grid(first=combos1, second=combos1)
combos2 = paste(substrings$first, substrings$second, sep="")
substrings = expand.grid(first=letters, second=letters, third=letters)
combos3 = paste(substrings$first, substrings$second, substrings$third, sep="")
combos3
combos = c(combos1, combos2, combos3)
length(combos)

# http://r.789695.n4.nabble.com/how-to-count-the-total-number-of-INCLUDING-overlapping-occurrences-of-a-substring-within-a-string-td975644.html
substringCount = function(substring, string) { 
	count = 0 
  i = 1; n = nchar(string) 
  found = regexpr(substring, substr(string, i, n), perl=TRUE) 
  while (found > 0) { 
    count = count + 1 
    i = i + found 
    found = regexpr(substring, substr(string, i, n), perl=TRUE) 
  } 
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
	dist = sqrt((x1$A - x2$A)^2 + (x1$C - x2$C)^2 + (x1$G - x2$G)^2 + (x1$T - x2$T))
}

gram = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in 1:n){
		gram[i,j] = kernel(train[i, ], train[j, ])
	}
	print(i)
}
save(gram, file="gram.rda")
load("gram.rda")

# b
rbf = function(x1, x2, sigma = 5){
	dist = kernel(x1, x2)
	k = exp(-(dist^2)/(2*sigma^2))
	return(k)
}

dim(train)
n = nrow(train)
K.train = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		K.train[i,j] = K.train[j,i] = rbf(train[i, ], train[j, ])
	}
	print(i)
}

test$A = test$C = test$G = test$T = NA 
for(i in 1:n){
	test$A[i] = letterCount("A", test$sequence[i])
	test$C[i] = letterCount("C", test$sequence[i])
	test$G[i] = letterCount("G", test$sequence[i])
	test$T[i] = letterCount("T", test$sequence[i])
}

n = nrow(test)
K.test = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		K.test[i,j] = K.test[j,i] = rbf(test[i, ], test[j, ])
	}
	print(i)
}

K = K.train 
head(K)
K.star.star = K.test

complete.data = rbind(train, test)
n = nrow(complete.data)
K.star = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		K.star[i,j] = K.star[j,i] = rbf(complete.data[i, ], complete.data[j, ])
	}
	print(i)
}

sigma = 5 

# problem inverting K 
alpha = solve(K) %*% train$intensity
#####################

# sigmaI = sigma^2 * diag(nrow(K))
# dim(sigmaI)
# dim(K)
# K.plus.sigmaI = K + sigmaI
# dim(K.plus.sigmaI)
# mu.star = t(K.star) %*% solve(K+sigmaI) 
# sigma.star = K.star.star - t(K.star) %*% solve(K) %*% K.star

intensity.est = mu.star * test$intensity