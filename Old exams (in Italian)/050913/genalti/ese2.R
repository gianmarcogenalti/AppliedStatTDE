rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('match.txt')
head(df)

mcshapiro.test(df) # pval = 0.2732 normali bivariati
shapiro.test(df[,1])
shapiro.test(df[,2]) # entrambe le colonne normali

D <- df

n <- dim(D)[1]  
p <- dim(D)[2]  

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(46,48)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p) # pvalue = 0.005956819 abbiamo motivo di affermare con confidenza 95% che l'arbitro
                                        # non stia rispettando gli standard

D$durtot <- D[,1]+D[,2]

n <- dim(D)[1]  
p <- dim(D)[2]  

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

IC.T2.DBOD <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.DSS  <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )
IC.T2.TOT  <- c( D.mean[3]-sqrt(cfr.fisher*D.cov[3,3]/n) , D.mean[3], D.mean[3]+sqrt(cfr.fisher*D.cov[3,3]/n) )

IC <- rbind(I = IC.T2.DBOD, II = IC.T2.DSS, FULL = IC.T2.TOT)

# I    45.41782 46.0500 46.68218
# II   46.67852 47.4235 48.16848
# FULL 92.20260 93.4735 94.74440




