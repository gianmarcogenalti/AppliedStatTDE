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


df = read.table("geisha.txt", header=T)
head(df)
dim(df)

# a)

df.e <- dist(df, method='euclidean')

help(hclust)

df.es <- hclust(df.e, method='single')
plot(df.es)
cl = cutree(df.es, k=2)
cl

plot(df)
plot(df, col=cl)

m1 = sapply(df[which(cl == 1),], mean)
m2 = sapply(df[which(cl == 2),], mean)

coph.es <- cophenetic(df.es)
cor(df.e, coph.es)


# b)

df.e <- dist(df, method='euclidean')

df.es <- hclust(df.e, method='average')
plot(df.es)
cl = cutree(df.es, k=2)
cl

plot(df)
plot(df, col=cl)

m1 = sapply(df[which(cl == 1),], mean)
m2 = sapply(df[which(cl == 2),], mean)

coph.es <- cophenetic(df.es)
cor(df.e, coph.es)

sum(cl == 2)
sum(cl == 1)





# c)

mcshapiro.test(df[which(cl == 1),])
mcshapiro.test(df[which(cl == 2),])


n1 <- dim(df[which(cl == 1),])[1] # n1=3
n2 <- dim(df[which(cl == 1),])[1] # n2=4
p  <- dim(df[which(cl == 1),])[2] # p=2

t1 = df[which(cl == 1),]
t2 = df[which(cl == 2),]

# we compute the sample mean, covariance matrices and the matrix
# Spooled

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .1
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# P-value high (we don't reject at 1%,5%,10%)

# Simultaneous T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2

