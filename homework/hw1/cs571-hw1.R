# CS 571: Machine Learning
# MCD, Duke U, Fall 2013

# Problem 2
(0.9*0.001)/(0.9*0.001 + 0.2*0.999) # 0.00448

# Problem 5
setwd('~/github/cs571/homework/hw1')
x = read.table("hw1.txt")
dim(x)
head(x)
class(x[1,1])

# a 
pdf("hist5a.pdf")
hist(x[,1], breaks=25, xlim=c(0,25),
	main="Histogram of X",
	xlab="X")
dev.off()

# b
gamma.mle = function(x, n){ return (x/n); }
mean(x[,1]) # 10.0171

# c-e

n = nrow(x)
gamma.map = function(alpha, beta){
	post = (alpha+sum(x[,1])-1)/(beta+n)
	return(post)
}
gamma.map(1, 1)
gamma.map(100, 1)
gamma.map(10, 1)