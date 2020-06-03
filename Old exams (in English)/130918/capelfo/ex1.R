library(car)
library(MASS)
library(class)

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


df = read.table("IAMG.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)

mcshapiro.test(df)

alpha <- 0.95

# a

df.mean   <- sapply(df,mean)
df.cov    <- cov(df)
df.invcov <- solve(df.cov)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)


# We add a red point in correspondence of the sample mean
plot(df.mean[1], df.mean[2], pch = 16, col ='red', cex = 1.5)

# Confidence region (centred in x.mean)
# { m \in R^2 s.t. n * (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }

ellipse(df.mean, df.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)





# b

T2 <- cbind(inf = df.mean - sqrt(cfr.fisher*diag(df.cov)/n),
            center = df.mean, 
            sup = df.mean + sqrt(cfr.fisher*diag(df.cov)/n))
T2


# c

# c)

t.test(0.1*df[,1] - df[,3], alternative = "two.sided", mu = 0)


