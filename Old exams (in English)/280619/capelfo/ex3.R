library(car)
library(MASS)
library(class)
library(mvtnorm)

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


df = read.table("airport.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

m = lm(duration ~ time.of.the.day + distance + distance:time.of.the.day)
summary(m)
linearHypothesis(m, rbind(c(1,0,0,0,0,0),c(0,1,0,0,0,0),c(0,0,1,0,0,0)), c(0,0,0))
linearHypothesis(m, rbind(c(0,0,0,1,0,0),c(0,0,0,0,1,0),c(0,0,0,0,0,1)), c(0,0,0))


m = lm(duration ~ -1 + distance + distance:time.of.the.day)
summary(m)

# prediction interval of 3-4 points