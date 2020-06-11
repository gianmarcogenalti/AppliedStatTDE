df <- read.table("pines.txt")


center <- c(14.235,42.452)
mcshapiro.test(df)
# barely gaussian omg


n <- dim(df)[1]
p <- dim(df)[2]

alpha <- 0.01

x.mean   <- colMeans(df)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)

x.T2 <- n * (x.mean-center) %*% x.invcov %*% (x.mean-center) 

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
x.T2 < cfr.fisher
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P
# quasi quasi rifiutiamo, che pelo


eigen(x.cov)$vectors


# Length of the semi-axes of the ellipse:
cfr.chisq <- qchisq(1-alpha,p)
r <- sqrt(cfr.chisq)
r*sqrt(eigen(x.cov)$values) 

library(car)
plot(df, xlim = c(14.2,14.3), ylim = c(42.4,42.5))
ellipse(center, shape=x.cov, sqrt(cfr.chisq), col = 'blue', lty = 2, center.pch = 4, center.cex=1.5, lwd=2)
points(center[1], center[2], pch = 4, cex = 1.5, lwd = 2, col ='blue')
