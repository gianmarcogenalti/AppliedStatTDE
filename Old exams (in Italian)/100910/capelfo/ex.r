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


df = read.table("horse.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)





m = qda(df[,c(1,2)], df[,3], priors=c(1/3, 1/3, 1/3))



# discrimination region
plot(df[,1:2], main='Plot', xlab='x1', ylab='x2', pch=20, col=df[,3])

points(m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(google=x, apple=y)

z  <- predict(m, xy)$post  
z1 <- z[,1] - (z[,2] + z[,3])
z2 <- z[,2] - (z[,1] + z[,3])
z3 = z[,3] - (z[,1] + z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)


mcv <- qda(df[,c(1,2)], df[,3], CV=T)
errorsqCV <- (mcv$class != df[,3])

AERqCV   <- sum(errorsqCV)/length(df[,3])
AERqCV




##############################################


df = read.table("pound.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)
