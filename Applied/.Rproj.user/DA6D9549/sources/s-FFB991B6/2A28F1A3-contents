df <- read.table('pigeons.txt')

# dati accoppiati  perché son fedeli

mcshapiro.test(df[,1:2])
mcshapiro.test(df[,3:4])
# 

n <- dim(df)[1]
p <- dim(df[,1:2])[2]
D <- data.frame(df[1]-df[3],df[2]-df[4])

mu0      <- c(0, 0)
x.mean   <- colMeans(D)
x.cov    <- cov(D)
x.invcov <- solve(x.cov)
alpha <- 0.05


x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher


P <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
P

# difference!

# b

alpha = 0.1
# Bonferroni -> sulle differenze? O sui 4?
k <- p 
x.mean <- sapply(D,mean)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf

# c

diff <- df[,2] - 1.2*df[,4]

t.test(diff,alternative = c("greater"),conf.level = 0.9)
