rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('cantabro.txt')
head(df)
mcshapiro.test(df)

D <- df
n <- dim(D)[1]  # 11
p <- dim(D)[2]  #  2

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(30,50)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

mcshapiro.test(D)

C <- rbind( c(1,0),     # only boys
            c(0,1),     # only girls
            c(1,1) )

mu <- colMeans(D) 
S <- cov(D)

alpha <- .01

T2 <- cbind( 
  C%*%mu - sqrt(diag(C%*%S%*%t(C))/n*(p*(n-1)/(n-p))*qf(1-alpha,p,n-p)),
  C%*%mu ,
  C%*%mu + sqrt(diag(C%*%S%*%t(C))/n*(p*(n-1)/(n-p))*qf(1-alpha,p,n-p)))
colnames(T2) <- c('Inf','Mean','Sup')
T2

