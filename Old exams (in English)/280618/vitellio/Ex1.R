rm(list = ls())
morning <- read.table("Morning.txt")
head(morning)
evening <- read.table("Evening.txt")
head(evening)

D <- data.frame(MEX.OAX=morning[,1]-evening[,1], OAX.MEX=morning[,2]-evening[,2]) 
D
load("mcshapiro.test.RData")
mcshapiro.test(D)

x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

n <- dim(D)[1]  
p <- dim(D)[2]

D.mean   <- sapply(D,mean)
D.cov    <-  cov(D)
D.invcov <- solve(D.cov)

alpha   <- .01
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher
D.T2 < cfr.fisher # FALSE: we reject at 5%

P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

# b)
k <- 4 # number of intervals I want to compute (set in advance)
alpha   <- .01
cfr.t <- qt(1-alpha/(2*k),n-1)
mcshapiro.test(morning)
mcshapiro.test(evening)
x.mean <- sapply(morning,mean)
x.mean <- c(x.mean, sapply(evening,mean))

x.cov <- c(var(morning$MEX.OAX), var(morning$OAX.MEX), var(evening$MEX.OAX), var(evening$OAX.MEX))
Bf <- NULL
for (i in 1:k)
  Bf <- rbind(Bf, cbind(inf = x.mean[i] - cfr.t*sqrt(x.cov[i]/n),
            center = x.mean[i],
            sup = x.mean[i] + cfr.t*sqrt(x.cov[i]/n)))
Bf


# c) 
t.test(morning$OAX.MEX, mu = 90, alternative = "greater")
