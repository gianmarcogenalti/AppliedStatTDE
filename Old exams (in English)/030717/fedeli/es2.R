mcshapiro.test <- function (X, devstmax = 0.01, sim = ceiling(1/(4 * devstmax^2))) 
{
  library(mvtnorm)
  library(mvnormtest)
  n <- dim(X)[1]
  p <- dim(X)[2]
  mu <- rep(0, p)
  sig <- diag(p)
  W <- NULL
  for (i in 1:sim) {
    Xsim <- rmvnorm(n, mu, sig)
    W <- c(W, mshapiro.test(t(Xsim))$stat)
  }
  Wmin <- mshapiro.test(t(X))$stat
  pvalue <- sum(W < Wmin)/sim
  devst <- sqrt(pvalue * (1 - pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, 
       sim = sim)
}

bento <- read.csv("C:/Users/ffede/OneDrive/Desktop/AppliedStatTDE/Old exams (in English)/030717/bento.txt", sep="")

mcshapiro.test(bento[,c(1,5)])
mcshapiro.test(bento[,c(2,6)])
mcshapiro.test(bento[,c(3,7)])
mcshapiro.test(bento[,c(4,8)])


n <- dim(bento)[1]
S <- cov(bento[,c(i,j)])
M <- sapply(bento,mean)

# Test: H0: C*mu=0 vs H1: C*mu!=0
Md <- C %*% M
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)
T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
