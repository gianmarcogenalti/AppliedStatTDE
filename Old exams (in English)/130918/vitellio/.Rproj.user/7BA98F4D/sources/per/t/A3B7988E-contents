rm(list = ls()) 
data = read.table("IAMG.txt")
head(data)
load("mcshapiro.test.RData")
n <- dim(data)[1]
p <- dim(data)[2]
mcshapiro.test(data)

m <- sapply(data, mean)
Sigma <- cov(data)
eigen(Sigma)
# Direction of the axes:
eigen(Sigma)$vectors

alpha <- 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(Sigma/n)$values) 



# b)
c1 <- m[1]
l1 <- m[1]-sqrt(cfr.fisher*Sigma[1,1]/n)
r1 <- m[1]+sqrt(cfr.fisher*Sigma[1,1]/n)
c2 <- m[2]
l2 <- m[2]-sqrt(cfr.fisher*Sigma[2,2]/n)
r2 <- m[2]+sqrt(cfr.fisher*Sigma[2,2]/n)
c3 <- m[3]
l3 <- m[3]-sqrt(cfr.fisher*Sigma[3,3]/n)
r3 <- m[3]+sqrt(cfr.fisher*Sigma[3,3]/n)

T2 <- cbind(inf = c(l1,l2,l3), center = c(c1,c2,c3), sup = c(r1,r2,r3))
T2

# c)
t.test(0.1*data$Registered - data$No.show, alternative = "two.sided", mu = 0)
