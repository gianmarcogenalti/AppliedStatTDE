rm(list = ls())
load('mcshapiro.test.RData')
inp <- read.table('risoimport.txt')
exp <- read.table('risoexport.txt')
head(inp)
head(exp)
dim(inp)
dim(exp)

mcshapiro.test(exp) # pval = 0.2156
mcshapiro.test(inp) # pval = 0.8812

cols = c()
for(i in 1:5){
  cols = cbind(cols, inp[,i]-exp[,i])
}
D <- data.frame(cols)
names(D) <- colnames(inp)
head(D)

mcshapiro.test(D) # pval = 0.9636

n <- dim(D)[1] 
p <- dim(D)[2]  

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0,0,0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
# reject H0 at 5% (reject at 1% also)

# there's difference
IC.T2 <- c()
for(i in 1:5){
  IC.T2 <- rbind( IC.T2, c(D.mean[i]-sqrt(cfr.fisher*D.cov[i,i]/n) , D.mean[i], D.mean[i]+sqrt(cfr.fisher*D.cov[i,i]/n)) )
}

#      inf        mid        sup
#   2.449096   2.503115   2.557134
#   2.436458   2.505383   2.574307
#  -33.044361 -32.985984 -32.927606
#   5.910443   6.011667   6.112891
#   6.963393   7.005246   7.047098

M.inp <- (inp[,1]+inp[,2]+inp[,3]+inp[,4]+inp[,5])/5
M.exp <- (exp[,1]+exp[,2]+exp[,3]+exp[,4]+exp[,5])/5
Dm <- M.inp - M.exp

n <- dim(D)[1] 
p <- 6 

D$diff <- Dm

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0,0,0,0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
IC.T2 <- c()
for(i in 1:6){
  IC.T2 <- rbind( IC.T2, c(D.mean[i]-sqrt(cfr.fisher*D.cov[i,i]/n) , D.mean[i], D.mean[i]+sqrt(cfr.fisher*D.cov[i,i]/n)) )
}
IC.T2
#   2.445402   2.503115   2.560827
#   2.431745   2.505383   2.579020
#   -33.048353 -32.985984 -32.923615
#   5.903522   6.011667   6.119811
#   6.960532   7.005246   7.049960
#   -3.038595  -2.992115  -2.945635
  