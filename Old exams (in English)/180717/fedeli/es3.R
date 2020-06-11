df <- read.table("castle.txt")

Aosta <- c(45.733,7.333)
mcshapiro.test(df)
# really gaussian much wow


n <- dim(df)[1]
p <- dim(df)[2]

alpha <- 0.05

x.mean   <- colMeans(df)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)

x.T2 <- n * (x.mean-Aosta) %*% x.invcov %*% (x.mean-Aosta) 

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
x.T2 < cfr.fisher
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P

eigen(x.cov)$vectors


# Length of the semi-axes of the ellipse:
cfr.chisq <- qchisq(1-alpha,p)
r <- sqrt(cfr.chisq)
r*sqrt(eigen(x.cov)$values) 

library(car)
plot(df, xlim = c(45.68,45.78), ylim = c(7.20,7.5))
ellipse(Aosta, shape=x.cov, sqrt(cfr.chisq), col = 'blue', lty = 2, center.pch = 4, center.cex=1.5, lwd=2)
points(Aosta[1], Aosta[2], pch = 4, cex = 1.5, lwd = 2, col ='blue')
