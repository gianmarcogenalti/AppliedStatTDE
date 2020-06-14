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


df = read.table("radioville.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
df$DD = 0
df[which(df$D == "U"),"DD"] = 1
attach(df)


coordinates(df) <- c('Long','Lat')
bubble(df,'Bq',do.log=F,key.space='bottom')

v <- variogram(Bq ~ DD, df)
plot(v, main = 'Sample Variogram',pch=19)
v.fit1 = fit.variogram(v, vgm(1, "Sph", 0.5))
plot(v, v.fit1)


# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
df.gstat1 <- gstat(formula = Bq ~ DD,
                     data = df, model=v.fit1)


#b

v.fit2 = fit.variogram(v, vgm(1, "Sph", 0.5, 0.1))
plot(v, v.fit2)


# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
df.gstat2 <- gstat(formula = Bq ~ DD,
                   data = df, model=v.fit2)


#c 



#d


s0.new <- as.data.frame(matrix(c(78.59,35.34,1),1,3))
names(s0.new) <- c('lon','lat','DD')
coordinates(s0.new) <- c('lon','lat')
predict(df.gstat1, s0.new)





