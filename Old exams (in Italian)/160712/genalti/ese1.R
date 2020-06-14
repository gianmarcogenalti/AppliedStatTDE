rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('cruise.txt')
df$total <- df[,1]+df[,2]+df[,3]+df[,4]
head(df)
mcshapiro.test(df)# pval = 0.9684 normali multivariati
pv <- c()
for(i in 1:5){
  pv <- c(pv, shapiro.test(df[,i])$p.value)
}
# pvalues = 0.6200412973, 0.5736335438, 0.7348879554, 0.4007748582, 0.0009974136

alpha <- .1

n <- dim(df)[1]
p <- dim(df)[2]

x.mean <- sapply(df,mean)
x.cov <- cov(df)

cfr.fisher <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)

ICT2 <- data.frame(L=x.mean-sqrt(diag(x.cov)/n)*sqrt(cfr.fisher),C=x.mean,U=x.mean+sqrt(diag(x.cov)/n)*sqrt(cfr.fisher))
#           L         C         U
# IngM   371.1984  396.9733  422.7483
# IngF   288.7855  315.9200  343.0545
# ArcM   274.4836  299.7733  325.0630
# ArcF   371.7102  397.8200  423.9298
# total 1375.0148 1410.4867 1445.9586

x <- df[,1] + df[,2] - df[,3] - df[,4]
t.test(x, conf.level = 0.95)# p-value = 0.202 possiamo confermare che il numero medio di architetti eguaglia quello degli ingegneri

y <- df[,1] + df[,3] - 2*(df[,2]+df[,4])
t.test(y)# p-value < 2.2e-16 dobbiamo rifiutare l'ipotesi nulla

df <- data.frame(x,y)
mu0 <- c(0,0)
alpha <- .1

n <- dim(df)[1]
p <- dim(df)[2]

x.mean <- sapply(df,mean)
x.cov <- cov(df)
x.invcov <- solve(x.cov)
# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)
# x.T2 = 491.5089
# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
# pvalue = 0 
