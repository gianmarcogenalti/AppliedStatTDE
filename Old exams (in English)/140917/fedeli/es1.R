df <- read.table('sunchair.txt')

C <- rbind(c(1,-1,0,0),c(1,0,-1,0),c(1,0,0,-1))

n = dim(df)[1]
p = dim(df)[2]

mcshapiro.test(df)
# normale

M = sapply(df, mean)
S = cov(df)

Md = C %*% M
Sd = C %*% S %*% t(C)
Sd.inv = solve(Sd)

n = dim(df)[1]
p = dim(df)[2]
mu0 = c(0,0,0)
T2   <- n * t(Md-mu0) %*%  Sd.inv  %*% (Md-mu0)
alpha = 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
P <- 1-pf(T2*(n-p)/((n-1)*p), p, n-p)
P
# rifiuto

# b

k     <- p   
alpha = 0.05
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( M - cfr.t*sqrt(diag(S)/n) , M, M + cfr.t*sqrt(diag(S)/n) )
IC.BF
