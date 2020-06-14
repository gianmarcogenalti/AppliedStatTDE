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


df = read.table("mmm.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)


a = aov(index ~ region + sandwich)
summary(a)



# Bonferroni
alpha <- 0.05
g <- 3
b <- 3
p <- 1
n <- 50
N <- n*g*b # 20

DF <- a$df # n*g*b - 1 - (g-1) = 15*3*2-1-2 = 90-3 = 87
Spooled <- sum(a$res^2)/DF #change name pls

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k

qT <- qt(1 - alpha / (2 * k), DF)
# the degrees of freedon of the residuals on the additive model are
# g*b*n-g-b+1

m1  <- mean(index[region=="Canada"])
m2  <- mean(index[region=="Europe"])
m3  <- mean(index[region=="USA"])
ma  <- mean(index[sandwich=="Bacon"])
mb  <- mean(index[sandwich=="Burger"])
mc  <- mean(index[sandwich=="Cheese"])

ic.1 = c(
  inf = m1 - m2 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m1 - m2 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.2 = c(
  inf = m1 - m3 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m1 - m3 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.3 = c(
  inf = m3 - m2 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m3 - m2 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.4 = c(
  inf = ma - mb - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = ma - mb + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.5 = c(
  inf = ma - mc - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = ma - mc + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.6 = c(
  inf = mc - mb - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = mc - mb + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
