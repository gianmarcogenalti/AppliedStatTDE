library(car)
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


df = read.table("kimono.txt", header=T)
head(df)
dim(df)

#a)

attach(df)
levels(city)
levels(type)
shapiro.test(value[which(city=="Kyoto")])
shapiro.test(value[which(city=="Tokyo")])
shapiro.test(value[which(type=="hand-made")])
shapiro.test(value[which(type=="ready-to-use")])


a = aov(value ~ city + type)
summary(a)

#b)

a = aov(value ~ type)
summary(a)

#c)
n = dim(df)[1]
ng = table(type)
p = 1
g = 2
k <- p*g*(g-1)/2
alpha= 0.05

Mediag  <- tapply(value, type, mean)
SSres <- sum(residuals(a)^2)
S <- SSres/(n-g)

# Example: CI for the difference "casein - horsebean"
as.numeric(c(Mediag[1]-Mediag[2] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] )),
             Mediag[1]-Mediag[2] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] ))))


