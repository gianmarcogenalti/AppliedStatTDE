rm(list = ls())
load('mcshapiro.test.Rdata')
mor <- read.table("Morning.txt")
eve <- read.table("Evening.txt")

D <- data.frame("mexoax" = mor[,1] - eve[,1], 'oaxmex' = mor[,2] - eve[,2])
mcshapiro.test(D)
# pvalue = 0.40 normale bivariata

mu0      <- c(0, 0)
x.mean   <- colMeans(D)
x.cov    <- cov(D)
x.invcov <- solve(x.cov)
n <- dim(D)[1]
p <- 2

x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
Pb <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
# pvalue = 0.00166 I accept the alternative hypothesis that there's a significative difference at level 1%

shapiro.test(mor[,1])
shapiro.test(mor[,2])
shapiro.test(eve[,1])
shapiro.test(eve[,2])
# normal datas

cfr.t <- qt(1-alpha/(2*k),n-1)

Md <- c(colMeans(mor), colMeans(eve))
Sd <- c(sapply(mor, var), sapply(eve, var))

IC.BF <- cbind(Md - cfr.t*sqrt(Sd/n) , Md, Md + cfr.t*sqrt(Sd/n) )

# MEX.OAX 21.37156 30.56290 39.75425
# OAX.MEX 29.59741 40.84161 52.08581
# MEX.OAX 36.04064 48.41194 60.78323
# OAX.MEX 45.26486 67.47258 89.68031

# intervallo che contiene il 99% dei ritardi oax-mex di mattina:
IC = c(qnorm(0.01, mean = Md[2], sd = sqrt(Sd[2])),qnorm(0.99, mean = Md[2], sd = sqrt(Sd[2])))

# [-3.293495; 84.976721] il 99% dei ritardi rientreranno in questo intervallo, quindi al 99% ce la farà
# a prendere il volo (max 90min di ritardo)



