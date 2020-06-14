library(car)
library(MASS)
library(class)
library(mvtnorm)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

mcshapiro.test <- function(X, devstmax = 0.01, sim = ceiling(1/(4*devstmax^2)))
{
  library(mvnormtest)
  n   <- dim(X)[1]
  p   <- dim(X)[2]
  mu  <- rep(0,p)
  sig <- diag(p)
  W   <- NULL
  for(i in 1:sim)
  {
    Xsim <- rmvnorm(n, mu, sig)
    W   <- c(W, mshapiro.test(t(Xsim))$stat)
    # mshapiro.test(X): compute the statistics min(W) for the sample X
  }
  Wmin   <- mshapiro.test(t(X))$stat   # min(W) for the given sample
  pvalue <- sum(W < Wmin)/sim          # proportion of min(W) more extreme than the observed Wmin
  devst  <- sqrt(pvalue*(1-pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, sim = sim)
}


df = read.table("pines.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

#a

mcshapiro.test(df)

df.mean   <- sapply(df,mean)
df.cov    <- cov(df)
df.invcov <- solve(df.cov)



### Test on the mean of level alpha=1% ######1
### H0: mu == mu0 vs H1: mu != mu0
### with mu0=c(1,0)
###-----------------------------------

alpha <- 0.01
mu0 <- c(14.2350,42.4520)
# T2 Statistics #????
x.T2       <- n * (df.mean-mu0) %*% df.invcov %*% (df.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P


# b

cfr.chisq <- qchisq(1-alpha,p)

# Characterize the ellipse:
# Axes directions:
eigen(df.cov)$vectors
# Center:

# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(df.cov)$values)  

plot(df)
points(df.mean[1], df.mean[2], pch = 4, cex = 1.5, lwd = 2)
ellipse(center=df.mean, shape=df.cov, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)

