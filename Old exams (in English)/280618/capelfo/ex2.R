library(car)
library(MASS)
library(class)
library(mvtnorm)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

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


df = read.table("Mexican.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

a = aov(price ~ type.food + area)
summary(a)

b = aov(price ~ area)
summary(b)

table(area)










# Test for independent Gaussian populations
df1 = df[area=="Cancun",1]
df2 = df[area=="Guanajato",1]
df3 = df[area=="MexicoCity",1]


t1.mean <- sapply(df1,mean)
t2.mean <- sapply(df2,mean)
t3.mean <- sapply(df2,mean)

n1 = length(df1)
n2 = length(df2)
n3 = length(df3)


t1.cov  <-  var(df1)
t2.cov  <-  var(df2)
t3.cov  <-  var(df3)

Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov + (n3-1)*t3.cov)/(n1+n2+n3-3)

# Test: H0: mu.1-mu.2==0 vs H1: mu.1-mu.2!=0
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P

### question c)
alpha <- 0.1
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC


