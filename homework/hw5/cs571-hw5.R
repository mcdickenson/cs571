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

substringOccurrences = function(string="AAAGTAAAGT", substring="AAA"){
	string_length = nchar(string)
	substring_length = nchar(substring)
	locations = seq(1, string_length-(substring_length-1), substring_length)
	substrings = NULL
	for(i in 1:substring_length){
		substrings = rbind(substrings, substring(string, locations+i-1, locations+substring_length+i-2))
	}
	match_locations = grep(substring, substrings)
	number_matches = length(match_locations)
	return(number_matches)
}

allSubstringCounts = function(string){
	s = 0 
	for(combo in combos){
		s = s + substringCount(combo, string)
	}
	return(s)
}

allSubstringOccurrences = function(string){
	s = 0 
	for(combo in combos){
		s = s + substringOccurrences(string=string, substring=combo)
	}
	return(s)
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
		gram[i,j] = gram[j,i] = stringKernel(train$sequence[i], train$sequence[j])
	}
	print(i)
}

save(gram, file="gram-new.rda")

load("gram.rda")
gram[1:10, 1:10]
plot(density(gram))

# b
rbf = function(x1, x2, sigma = 5){
	# dist = (allSubstringCounts(x1) - allSubstringCounts(x2))^2
	dist = stringKernel(x1, x2)
	k = exp(-dist/(2*sigma^2))
	return(k)
}

rbfFromStringCount = function(dist, sigma = 5){
	# dist = (allSubstringCounts(x1) - allSubstringCounts(x2))^2
	k = exp(-dist/(2*sigma^2))
	return(k)
}

# train$sequence[1]
# train$sequence[2]
# substringCount("TTT", train$sequence[1])
# substringCount("TTT", train$sequence[2])

# allSubstringCounts(train$sequence[1])
# allSubstringCounts(train$sequence[2])

# rbf(train$sequence[1], train$sequence[2])

dim(train)
n = nrow(train)
head(train)
# n = 10 
K.train = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		dist = gram[i,j]
		K.train[i,j] = K.train[j,i] = rbfFromStringCount(dist)
	}
	print(i)
}
K.train[1:10, 1:10]
save(K.train, file="K-train.rda")

n = nrow(test)
K.test = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		K.test[i,j] = K.test[j,i] = rbf(test$sequence[i], test$sequence[j])
	}
	print(i)
}
K.test[1:10, 1:10]
save(K.test, file="K-test.rda")

K = K.train 
head(K)
K.star.star = K.test

complete.data = rbind(train, test)
head(complete.data)
n = nrow(complete.data)
K.star = matrix(NA, nrow=n, ncol=n)
for(i in 1:n){
	for(j in i:n){
		K.star[i,j] = K.star[j,i] = rbf(complete.data$sequence[i], complete.data$sequence[j])
	}
	print(i)
}
save(K.star, file="K-star.rda")

sigma = 5 

# problem inverting K 
alpha = solve(K) %*% train$intensity
#####################

sigmaI = sigma^2 * diag(nrow(K))
dim(sigmaI)
dim(K)
K.plus.sigmaI = K + sigmaI
# dim(K.plus.sigmaI)
# mu.star = K.star %*% solve(K) 
# sigma.star = K.star.star - t(K.star) %*% solve(K) %*% K.star
# intensity.est = mu.star * test$intensity



##################
FULL = K.star
k.x.x = FULL[1:1000, 1:1000]
k.x.x.noisy = k.x.x + sigmaI
k.xstar.x = FULL[1001:1100, 1:1000]
dim(k.xstar.x)
f.star = k.xstar.x %*% t(k.x.x.noisy) %*% train$intensity
f.star[1:10, 1:10]
dim(f.star)
f.star

summary(train$intensity)
k.xstar.x[1:10, 1:10]


#####
k.star = FULL[1:1000, 1001:1100]
mu.star = t(k.star) %*% solve(k.x.x) %*% train$intensity
dim(mu.star)
mu.star[1:10,1]

ests = mu.star


plot(test$intensity, ests)
