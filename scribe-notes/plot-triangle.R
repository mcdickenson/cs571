x = c(-1, 0, 1)
y = c(1, -1, 1)

z = seq(-1, 1, length.out=100)
z2 = z^2

pdf("triangle.pdf")
plot(x, y, 
	pch=16,
	xlim=c(-1.5, 1.5),
	ylim=c(-1.5, 1.5),
	xlab="",
	ylab="")
polygon(x, y, col="grey85")
text(0.2, -1.1, "(0,-1)")
text(1.2, 1.1, "(1,1)")
text(-1.2, 1.1, "(-1, 1)")
lines(z, z2)
abline(h=0)
abline(v=0)
dev.off()