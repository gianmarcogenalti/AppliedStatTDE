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


df1 = read.table("Morning.txt", header=T)
df2 = read.table("Evening.txt", header=T)
df = cbind(df, df2)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

# a

mcshapiro.test(df)

D <- data.frame(a=df[,1]-df[,3], b=df[,2]-df[,4]) 
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

mcshapiro.test(D)


# test for the mean
n <- dim(D)[1]
p <- dim(D)[2]

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .01
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 1%



# b


df.mean = sapply(df, mean)
df.cov = cov(df)
n = dim(df)[1]
p = dim(df)[2]
alpha = 0.01
# Let's try with Bonferroni intervals ##########4
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = df.mean - cfr.t*sqrt(diag(df.cov)/n),
            center = df.mean, 
            sup = df.mean + cfr.t*sqrt(diag(df.cov)/n))
Bf


# c

t.test(df[,2], mu = 90, alternative = "greater", conf.level = 0.01)

