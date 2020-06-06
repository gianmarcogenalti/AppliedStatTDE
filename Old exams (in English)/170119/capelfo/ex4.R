library(car)
library(MASS)
library(mvtnorm)
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


df = read.table("areaC.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)
m = lm(Total ~ Weekend + Petrol + Diesel + Electric + GPL + Natural.Gas + Hybrid.Diesel + Hybrid.Petrol)
summary(m)

plot(m)
hist(Total)


# c

### A possible solution to collinearity: PCA
df.pc <- princomp(df[,c(1,2,3,4,5,6,7,9)], scores=TRUE)
summary(df.pc)
df.pc$load

df.load = df.pc$loadings

par(mfcol = c(4,2))
for(i in 1:8) barplot(df.load[,i], ylim = c(-1, 1), main=paste("PC",i))

# d
fm.pc <- lm(Total ~ df.pc$scores[,1:5])

summary(fm.pc) 


