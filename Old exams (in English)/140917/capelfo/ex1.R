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


df = read.table("sunchair.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)

mcshapiro.test(df)

a = matrix(
  c(-1, 1, 0, 0,
    -1, 0, 1, 0,
    -1, 0, 0, 1), 3, 4, byrow = T
)

M = sapply(df, mean)
S = cov(df)

Mc = a %*% M
Sc = a %*% S %*% t(a)
ISc = solve(Sc)

n = dim(df)[1]
p = dim(df)[2]
mu0 = c(0,0,0)
T2   <- n * t(Mc-mu0) %*%  ISc  %*% (Mc-mu0)
alpha = 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
P <- 1-pf(T2*(n-p)/((n-1)*p), p, n-p)
P



# b

# Bonferroni intervals 
k     <- p   # number of increments (i.e., dim(C)[1])
alpha = 0.05
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( M - cfr.t*sqrt(diag(S)/n) , M, M + cfr.t*sqrt(diag(S)/n) )
IC.BF
