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


df = read.table("match.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

# a
mcshapiro.test(df)

x.mean = sapply(df, mean)
x.cov = cov(df)
x.invcov = solve(x.cov)

alpha <- 0.05
mu0 <- c(46,48)
# T2 Statistics #????
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
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

a = matrix(
  c(1, 0,
    0, 1,
    1, 1), 3, 2, byrow = T
)
p = 3
alpha = 0.05
Mc = a %*% x.mean
Sc = a %*% x.cov %*% t(a)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)


IC.T2 <- cbind( Mc - sqrt(cfr.fisher*diag(Sc)/n) , Mc, Mc + sqrt(cfr.fisher*diag(Sc)/n) )
IC.T2
