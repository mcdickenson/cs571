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

l2norm1 = sqrt(sum(eigen1^2))
l2norm2 = sqrt(sum(eigen2^2))
l2norm3 = sqrt(sum(eigen3^2))

pdf("1a.pdf")
par(mfrow=c(1,3))
plot(density(eigen1/l2norm1), main="X1")
plot(density(eigen2/l2norm2), main="X2")
plot(density(eigen3/l2norm3), main="X3")
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

# c 
mean(eigen1/l2norm1)
mean(eigen2/l2norm2)
mean(eigen3/l2norm3)

sd(eigen1/l2norm1)
sd(eigen2/l2norm2)
sd(eigen3/l2norm3)



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
substrings = expand.grid(first=combos1, second=combos1, third=combos1)
combos3 = paste(substrings$first, substrings$second, substrings$third, sep="")
combos3
combos = c(combos1, combos2, combos3)
length(combos)

substringCount = function(substring, string) { 
	count = 0 
  i = 1 
  n = nchar(string) 
  found = regexpr(substring, substr(string, i, n), perl=TRUE) 
  while (found > 0) { 
    count = count + 1 
    i = i + found 
    found = regexpr(substring, substr(string, i, n), perl=TRUE) 
  } 
  return(count) 
}


charsInCombos = 4+(2*16)+(3*64)

stringKernel = function(x1, x2){
	s = 0 
	for(combo in combos){
		phi = substringCount(combo, x1) * substringCount(combo, x2)
		w = nchar(combo)/charsInCombos
		s = s + (w*phi)
	}
	return(s)
}

n = nrow(train)
gram = matrix(NA, nrow=n, ncol=n)
start = proc.time()
for(i in 1:n){
	for(j in i:n){
		gram[i,j] = gram[j,i] = 
			stringKernel(train$sequence[i], train$sequence[j])
	}
	print(i)
}


# b
rbf = function(x1, x2, sigma = 5){
	dist = stringKernel(x1, x2)
	k = exp(-dist/(2*sigma^2))
	return(k)
}

complete.data = rbind(train, test)
head(complete.data)
n = nrow(complete.data)
rbfMatrix = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		rbfMatrix[i,j] = rbfMatrix[j,i] = 
			rbf(complete.data$sequence[i], complete.data$sequence[j])
	}
	print(i)
}
save(K.star, file="K-star.rda")

sigma = 5 

sigmaI = sigma^2 * diag(nrow(K))
FULL = K.star
k.x.x = FULL[1:1000, 1:1000]
k.x.x.noisy = k.x.x + sigmaI

k.star = FULL[1:1000, 1001:1100]
mu.star = t(k.star) %*% solve(k.x.x) %*% train$intensity
dim(mu.star)
mu.star[1:10,1]

ests = mu.star
regline = lm(ests ~ test$intensity)

RMSE(ests, test$intensity)

pdf("scatterplot.pdf")
plot(test$intensity, ests,
	xlab="Observed Intensity",
	ylab="Predicted Intensity")
abline(regline)
dev.off()
