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


df = read.table("buoys.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)



df.e <- dist(df[,1:2], method='euclidean')

clustw <- hclust(df.e, method='ward.D2')


par(mfrow=c(1,3))

plot(clustw, main='euclidean-wd2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')


cl <- cutree(clustw, k=3) # euclidean-complete:
cl

plot(df[,1:2], col=cl)

table(cl)


# b

fit <- aov(DO ~ cl)



### We reject the test, i.e., we have evidence to state that the  
### treatment (feed supplement) has an effect on the growth rate
### of chicken.
### Which supplement is responsible for this? To see this, we need to 
### do g*(g-1)/2 comparisons.
### One Way ANOVA We use Bonferroni ###########8
g = 3
k <- 1*g*(g-1)/2

Mediag  <- tapply(DO, cl, mean)
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)

ng = table(cl)


P <- matrix(0,3,3)
for(i in 1:3) {
  for(j in i:3) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  for(j in 1:i) {
    P[i,j] <- (1-pt(abs((Mediag[i]-Mediag[j]) / sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2}
  P[i,i]     <- 0
}
P



