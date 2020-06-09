mcshapiro.test <- function (X, devstmax = 0.01, sim = ceiling(1/(4 * devstmax^2))) 
{
  library(mvtnorm)
  library(mvnormtest)
  n <- dim(X)[1]
  p <- dim(X)[2]
  mu <- rep(0, p)
  sig <- diag(p)
  W <- NULL
  for (i in 1:sim) {
    Xsim <- rmvnorm(n, mu, sig)
    W <- c(W, mshapiro.test(t(Xsim))$stat)
  }
  Wmin <- mshapiro.test(t(X))$stat
  pvalue <- sum(W < Wmin)/sim
  devst <- sqrt(pvalue * (1 - pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, 
       sim = sim)
}

bento <- read.csv("C:/Users/ffede/OneDrive/Desktop/AppliedStatTDE/Old exams (in English)/030717/bento.txt", sep="")
attach(bento)

diff <- data.frame(rice = rice_hanami - rice_nohanami, sashimi = sashimi_hanami - sashimi_nohanami, vegetables = 
                           vegetables_hanami - vegetables_nohanami, okashi = okashi_hanami - okashi_nohanami)

mcshapiro.test(diff)
#they are gaussian

means <- colMeans(diff)
Sd <- cov(diff)
n <- dim(diff)[1]
p <- 4
alpha = 0.05


test_stat <- n*t(means)%*%solve(Sd)%*%means
p_val <- 1 - pf((n-p)/((n-1)*p)*test_stat, p, n-p)
p_val

# data are gaussian and they are different

sim <- function(a){
    return(c(inf = t(a)%*%means - sqrt((n-1)*p/(n-p)*qf(1 - alpha, p, n-p))*sqrt(t(a)%*%Sd%*%a/n), sup = t(a)%*%means + sqrt((n-1)*p/(n-p)*qf(1 - alpha, p, n-p))*sqrt(t(a)%*%Sd%*%a/n)))
}

print(sim(c(1,0,0,0)))
print(sim(c(0,1,0,0)))
print(sim(c(0,0,1,0)))
print(sim(c(0,0,0,1)))

# evidenza per 2 e per 4