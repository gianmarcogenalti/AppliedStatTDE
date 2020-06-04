load("mcshapiro.test.RData")
df <- read.table("luggage.txt")
head(df)

# male vs female
D <- data.frame(out = df[,1]-df[,2], ret = df[,3]-df[,4])
mcshapiro.test(D) # normal bivariate with pval = 0.2784

n <- dim(D)[1]  
p <- 2

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .1
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

### Simultanouse T2 intervals
p<-4
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
IC.T2.male <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.female <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )


# there's difference between males and females with a high confidence level

# out vs ret
D <- data.frame(males = df[,1] - df[,3], females = df[,2] - df[,4])
mcshapiro.test(D) # normal bivariate with pval = 0.1332

n <- dim(D)[1]  
p <- dim(D)[2]

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .1
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P

### Simultanouse T2 intervals
p<-4
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
IC.T2.out <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.ret  <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )


# there's difference between outbound and return with a high confidence level

IC <- rbind(IC.T2.male, IC.T2.female, IC.T2.out, IC.T2.ret)
#                 lwr       mid       upr
# IC.T2.male   -4.713355 -4.339052 -3.964749
# IC.T2.female -4.344776 -3.991897 -3.639017
# IC.T2.out    -3.069034 -2.714483 -2.359931
# IC.T2.ret    -2.749345 -2.367328 -1.985310

# we can see how every interval does not even intercept 0, so there are evidences supporting the tests previously
# made: all the differences are different from 0

shapiro.test(df[,4])
# we have normal univariate data, let's calculate IC
t.test(df[,4], conf.level = 0.9)
# One Sample t-test
# 
# data:  df[, 4]
# t = 203.16, df = 231, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 90 percent confidence interval:
#   20.91715 21.26000
# sample estimates:
# mean of x 
# 21.08858 

# the confidence interval does not reach 23 kg even with the upper bound, so I can say that she will not pay
# the fee



