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


df = read.table("montserrat.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

## Define the sample coordinates
coordinates(df) <- c('x','y')

# bubble plot(obj,zcol,...)
# key.space=location of the key
bubble(df,'speed',do.log=F,key.space='bottom')

v=variogram(speed ~ 1, data=df)
plot(v,pch=19)

v.t=variogram(speed ~ distance, data=df)
plot(v.t,pch=19)

plot(distance, speed)


# Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
v.fit1 <- fit.variogram(v.t, vgm(8, "Sph", 25))
plot(v.t, v.fit1, pch = 3)
v.fit1

# c

g.t <- gstat(formula = speed ~ distance, data = df, model = v.fit1)

# d

g.t <- gstat(formula = f ~ D, data = data, model = v.fit2)

D.s0=0
s0=as.data.frame(matrix(c(402476,4605558,D.s0),1,3))
names(s0)=c('x','y','distance')
coordinates(s0)=c('x','y')

predict(g.t, s0, BLUE = FALSE)
