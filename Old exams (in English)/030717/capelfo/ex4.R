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


df = read.table("garden.txt", header=T)
head(df)
dim(df)


mod = lm(formula=extension ~ .-extension, data=df)
plot(mod)

summary(mod)
sigma = var(mod$residuals)

# b)

linearHypothesis(mod, rbind(c(0,0,0,1,0), c(0,0,1,0,0)), c(0,0))
linearHypothesis(mod, rbind(c(0,1,0,0,1)), c(0))
linearHypothesis(mod, rbind(c(0,0,0,0,1), c(0,1,0,0,0)), c(0,0))
linearHypothesis(mod, c(0,0,0,1,0), c(0))

# c)

mod2 = lm(formula=extension ~ .-extension-cherry-carps, data=df)
summary(mod2)
