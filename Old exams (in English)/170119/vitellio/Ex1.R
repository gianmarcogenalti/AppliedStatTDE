rm(list = ls())
data <- read.table("luggage.txt")
head(data)
n <- dim(data)[1]

# a)
D1 <- data.frame(out=data[,1]-data[,2], ret=data[,3]-data[,4]) 
D1
load("mcshapiro.test.RData")
mcshapiro.test(D1)
p <- dim(D1)[2]
D.mean   <- sapply(D1,mean)
D.cov    <-  cov(D1)
D.invcov <- solve(D.cov)

delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)

P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

# b)
D2 <- data.frame(man=data[,1]-data[,3], woman=data[,2]-data[,4]) 
mcshapiro.test(D2)

D.mean2   <- sapply(D2,mean)
D.cov2    <-  cov(D2)
D.invcov2 <- solve(D.cov2)

delta.0 <- c(0,0)

D.T22 <- n * (D.mean2-delta.0) %*% D.invcov2 %*% (D.mean2-delta.0)

P <- 1-pf(D.T22*(n-p)/(p*(n-1)), p, n-p)
P


# c)
alpha <- 0.1
k <- 4
p <- 4
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

IC <- rbind(c(D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n), 
              D.mean[1], 
              D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n)),
            c(D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n), 
              D.mean[2], 
              D.mean[2]+sqrt(cfr.fisher*D.cov[1,1]/n) ),
            c(D.mean2[1]-sqrt(cfr.fisher*D.cov2[2,2]/n), 
              D.mean2[1], 
              D.mean2[1]+sqrt(cfr.fisher*D.cov2[2,2]/n) ),
            c(D.mean2[2]-sqrt(cfr.fisher*D.cov2[2,2]/n), 
              D.mean2[2], 
              D.mean2[2]+sqrt(cfr.fisher*D.cov2[2,2]/n) ) )
IC

# d)
t.test(data$F.ret, conf.level = 0.9, mu = 23, alternative = "greater")
