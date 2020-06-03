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


df = read.table("mickey.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)
tourists = 1 + cos(4*pi/365*day)

m = lm(waiting.time ~ day.of.the.week + tourists:day.of.the.week + tourists  )
summary(m)

# b

linearHypothesis(m, rbind(c(0,1,0,1)), c(0))

# c

m = lm(waiting.time ~ day.of.the.week + tourists  )
summary(m)

# d

plot(day, waiting.time) #????

# e


Z0.new <- data.frame(day=238, day.of.the.week="weekdays", tourists = 1 + cos(4*pi/365*238) )

# Conf. int. for the mean
Conf <- predict(m, Z0.new, interval='confidence', level=1-0.05)  
Conf
# Pred. int. for a new obs
Pred <- predict(m, Z0.new, interval='prediction', level=1-0.05)  
Pred
