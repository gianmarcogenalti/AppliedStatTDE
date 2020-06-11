df <- read.table("Revenues.txt")
attach(df)

diff <- data.frame(Jul - Jun, Aug - Jul, Sept - Aug)
mcshapiro.test(diff)
# le differenze sono normali

means <- colMeans(diff)
Sd <- cov(diff)
n <- dim(diff)[1]
p <- length(diff)


test.stat <- n*t(means)%*%solve(Sd)%*%means
p.val <- 1 - pf((n-p)/((n-1)*p)*test.stat, p, n-p)
p.val
#they are different 

alpha = 0.1
p <- 4
means <- colMeans(df)
Sd <- cov(df)
mcshapiro.test(df)
#gaussian

sim <- function(a){
  return(c(inf = t(a)%*%means - sqrt((n-1)*p/(n-p)*qf(1 - alpha, p, n-p))*sqrt(t(a)%*%Sd%*%a/n), sup = t(a)%*%means + sqrt((n-1)*p/(n-p)*qf(1 - alpha, p, n-p))*sqrt(t(a)%*%Sd%*%a/n)))
}


print(sim(c(1,0,0,0)))
print(sim(c(0,1,0,0)))
print(sim(c(0,0,1,0)))
print(sim(c(0,0,0,1)))
