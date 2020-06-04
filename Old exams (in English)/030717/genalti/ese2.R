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

library(car)


df <- read.table('bento.txt')
mcshapiro.test(df[,1:4])
mcshapiro.test(df[,5:8])

#####################################################################################################################
D <- data.frame(rice = df[,1]-df[,5], sashimi = df[,2]-df[,6], vegetables = df[,3]-df[,7], okashi = df[,4]-df[,8])
head(D)

matplot(D, type = 'l')

x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0, 0, pch=19, col='grey35')

# Now we can proceed as we already know, but working on D

### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

# Test the Gaussian assumption (on D!)
mcshapiro.test(D)


n <- dim(D)[1] # 32
p <- dim(D)[2] # 4

D.mean   <- sapply(D,mean)
D.mean
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0,0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha/p,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%
# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

# l'hanami impatta sui bento delle famiglie

# Now, let's communicate our results to the client.
# Let's build confidence intervals for linear combination of the
# components of the mean vector

### Simultanouse T2 intervals
IC.T2.rice <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.sashimi  <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )
IC.T2.vegetables  <- c( D.mean[3]-sqrt(cfr.fisher*D.cov[3,3]/n) , D.mean[3], D.mean[3]+sqrt(cfr.fisher*D.cov[3,3]/n) )
IC.T2.okashi  <- c( D.mean[4]-sqrt(cfr.fisher*D.cov[4,4]/n) , D.mean[4], D.mean[4]+sqrt(cfr.fisher*D.cov[4,4]/n) )

T2 <- rbind(IC.T2.rice, IC.T2.sashimi, IC.T2.vegetables, IC.T2.okashi)
dimnames(T2)[[2]] <- c('inf','center','sup')
T2



