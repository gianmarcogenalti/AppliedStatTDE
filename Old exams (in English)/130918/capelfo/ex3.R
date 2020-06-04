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


df = read.table("Sailing.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)


# a


qda.df <- qda(df[,c(1,2)], df[,3], prior=c(0.2, 0.8)) # x, grouping

plot(df[,1:2], main='DF', col=df[,3], xlab='water', ylab='time', pch=20)

points(qda.df$means, pch=4 , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(V1=x, V2=y)

z  <- predict(qda.df, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)



# Compute the estimate of the AER by leave-out-out cross-validation 
QdaCV.iris <- qda(iris2, species.name, CV=T)
QdaCV.iris$class
species.name
table(class.true=species.name, class.assignedCV=QdaCV.iris$class)

errorsqCV <- (QdaCV.iris$class != species.name)
errorsqCV

AERqCV   <- sum(errorsqCV)/length(species.name)
AERqCV