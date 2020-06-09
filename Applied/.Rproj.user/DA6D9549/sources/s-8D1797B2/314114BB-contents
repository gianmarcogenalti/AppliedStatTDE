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


df = read.table("Focaccia.txt", header=T)
head(df)
dim(df)

attach(df)

m = lm(kg ~ day + day:t + t )
summary(m)


plot(m)

linearHypothesis(m, rbind(c(1,1,0,0)), c(0))
linearHypothesis(m, rbind(c(0,0,0,1)), c(0))


# d)

m2 = lm(kg ~ day + t )
summary(m2)

linearHypothesis(m2, rbind(c(0,1,0)), c(60))

# e)

Z0.new <- data.frame(t=61, day="weekday")

# Conf. int. for the mean
Conf <- predict(m2, Z0.new, interval='confidence', level=1-0.05)  
Conf
# Pred. int. for a new obs
Pred <- predict(m2, Z0.new, interval='prediction', level=1-0.05)  
Pred




