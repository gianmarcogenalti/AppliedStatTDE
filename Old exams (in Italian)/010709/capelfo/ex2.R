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


df = read.table("index.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
df[,"eg"] = ifelse((df[,"Anno"] <= 1815) & (df[,"Anno"] >= 1768), 1, 0)
table(df[,"eg"])
attach(df)


ma = lm(Numero ~ Anno)
mb = lm(Numero ~ eg + Anno + Anno:eg)

summary(ma)
summary(mb)



# c

k <- 2
alpha <- .1
r <- 3 ###???

Z0   <- data.frame(eg=1, Anno=1800)
ICBmean <- predict(mb, Z0, interval='confidence',level=1-alpha/k) 
ICBmean

e <- residuals(mb)
ICBvar <- data.frame(L=t(e)%*%e/qchisq(1-alpha/(2*k),n-(r+1)),
                     U=t(e)%*%e/qchisq(alpha/(2*k),n-(r+1)))
ICBvar



# d

### question d)
a <- c(0,-1,1,-1815)
Bf <- c('1816-1815_L'= t(a) %*% coefficients(mb) - sqrt(t(a) %*% vcov(mb) %*% a) * qt(1 - alpha/2, n-(r+1)),
        '1816-1815_U'= t(a) %*% coefficients(mb) + sqrt(t(a) %*% vcov(mb) %*% a) * qt(1 - alpha/2, n-(r+1)) )
Bf

