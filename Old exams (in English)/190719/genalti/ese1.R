rm(list = ls())
load('mcshapiro.test.Rdata')
df <- read.table('pines.txt')
head(df)
#Long = 14:2350, Lat = 42:4520
plot(df[,1], df[,2])

mcshapiro.test(df)
# i dati sono normali bivariati

mu0      <- c(14.2350, 42.4520)
x.mean   <- colMeans(df)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)
n <- dim(df)[1]
p <- 2

x.T2  <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
Pb    <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
Pb
# pvalue = 0.01502446 > 0.01
# con confidenza del 99% possiamo (al pelo) affermare che il centro della pineta è nelle nostre coordinate

alpha <- 0.01
cfr.chisq <- qchisq(1-alpha,p)

# Characterize the ellipse:
# Axes directions:
eigen(x.cov)$vectors
# Center:
x.mean
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(x.cov)$values)  

x11()
plot(df, asp = 1, col='forestgreen', pch=19)
points(x.mean[1], x.mean[2], pch = 4, cex = 1.5, lwd = 2)

ellipse(center=x.mean, shape=x.cov, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)


graphics.off()
