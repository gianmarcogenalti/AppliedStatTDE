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


df = read.table("Lumieres.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)

# a

daysq = day^2
m = lm(N.people ~ rain + day + daysq + temperature)
summary(m)
var(residuals(m))


# b

linearHypothesis(m, c(0,0,1,1,0), c(0))


# c

m = lm(N.people ~ day + daysq)
summary(m)

# d e ???



