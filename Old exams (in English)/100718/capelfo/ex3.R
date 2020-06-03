library(car)

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


df = read.table("Presales.txt", header=T)
ef = read.table("Sales.txt", header=T)
head(df)
dim(df)

head(ef)
dim(ef)

mcshapiro.test(df)
mcshapiro.test(ef)

D = ef - df

# a)

n <- dim(D)[1]  # 11
p <- dim(D)[2]  #  2

df.mean = sapply(df, mean)
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .01
delta.0 <- -df.mean * 0.2

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%



# b)

D.mean
D.cov

# c

alpha   <- .01

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)


plot(D.mean[1], D.mean[2], pch = 16, col ='red', cex = 1.5, ylim=c(-100,100), xlim=c(-100,100))

cfr.chisq <- qchisq(1-alpha,p)

ellipse(center=D.mean, shape=D.cov, radius=sqrt(cfr.chisq), col = 'red', lty = 2, lwd=2, center.cex=1)

eigen(D.cov)$vectors
# Center:
D.mean

# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(S)$values)  
