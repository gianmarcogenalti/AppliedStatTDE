rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('price.txt')
head(df)
gas <- df
mcshapiro.test(gas)



n <- dim(gas)[1]
p <- dim(gas)[2]

alpha <- .1
M <- sapply(gas,mean)
M
S <- cov(gas)
Sinv <- solve(S)



cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

ICT2 <- data.frame(L=M-sqrt(diag(S)/n)*sqrt(cfr.fisher),C=M,U=M+sqrt(diag(S)/n)*sqrt(cfr.fisher))
ICT2

C <- rbind(c(2,-1,0),c(-1,2,0))
q <- dim(C)[1]
delta0 <- c(0,0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t(Md-delta0) %*% Sdinv %*% (Md-delta0)
pvalue <- 1- pf(T2*(n-q)/((n-1)*q),q,n-q)
pvalue

