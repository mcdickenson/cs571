# CS 571, Duke University, Fall 2013
# Matt Dickenson - mcd31
# Homework 6

setwd('~/github/cs571/homework/hw7')

# Problem 2
beta02 = rbeta(1000, 0.2, 0.2)
beta1  = rbeta(1000, 1, 1)
beta10 = rbeta(1000, 10, 10)

pdf("betasamps.pdf", height=6, width=2)
par(mfrow=c(3,1))
hist(beta02, main="", xlab="alpha=(0.2, 0.2)")
hist(beta1, main="", xlab="alpha=(1, 1)")
hist(beta10, main="", xlab="alpha=(10, 10)")
dev.off()