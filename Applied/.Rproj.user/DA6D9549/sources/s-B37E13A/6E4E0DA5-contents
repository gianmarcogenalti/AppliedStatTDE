rm(list = ls ())
data <- read.table(file = "castle.txt")
load("mcshapiro.test.RData")
head(data)
summary(data)
mcshapiro.test(data)

n <- dim(data)[1]
p <- dim(data)[2]
center <- c(Lat = 45.733, Long = 7.333)

alpha <- 0.05

x.mean   <- colMeans(data)
x.cov    <- cov(data)
x.invcov <- solve(x.cov)

x.T2       <- n * (x.mean-center) %*% x.invcov %*% (x.mean-center) 

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
plot(data, xlim = c(45.68,45.78), ylim = c(7.20,7.5))
ellipse(center, shape=x.cov, sqrt(cfr.chisq), col = 'blue', lty = 2, center.pch = 4, center.cex=1.5, lwd=2)
points(center[1], center[2], pch = 4, cex = 1.5, lwd = 2, col ='blue')
