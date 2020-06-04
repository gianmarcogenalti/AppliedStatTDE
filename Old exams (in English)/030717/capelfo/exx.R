library(car)
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


df = read.table("bento.txt", header=T)
head(df)
dim(df)

# a)

d = df[,c(1,2,3,4)] - df[,c(5,6,7,8)]


mcshapiro.test(d)


# b)

d.mean   <- sapply(d,mean)
d.cov    <- cov(d)
d.invcov <- solve(d.cov)

n <- dim(d)[1]  # 11
p <- dim(d)[2]  #  2


alpha   <- .05

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)


### Simultanouse T2 intervals
IC.rice <- c( d.mean[1]-sqrt(cfr.fisher*d.cov[1,1]/n) , d.mean[1], d.mean[1]+sqrt(cfr.fisher*d.cov[1,1]/n) )
IC.sashimi  <- c( d.mean[2]-sqrt(cfr.fisher*d.cov[2,2]/n) , d.mean[2], d.mean[2]+sqrt(cfr.fisher*d.cov[2,2]/n) )
IC.vegetables  <- c( d.mean[3]-sqrt(cfr.fisher*d.cov[3,3]/n) , d.mean[3], d.mean[3]+sqrt(cfr.fisher*d.cov[3,3]/n) )
IC.okashi  <- c( d.mean[4]-sqrt(cfr.fisher*d.cov[4,4]/n) , d.mean[4], d.mean[4]+sqrt(cfr.fisher*d.cov[4,4]/n) )

