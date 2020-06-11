df1 <- read.table('girona.txt')
df2 <- read.table('terrassa.txt')

mcshapiro.test(df1)
mcshapiro.test(df2)

df <- df1 -df2

mcshapiro.test(df)

n <- dim(df)[1]
p <- dim(df)[2]
mus <- colMeans(df)
S <- var(df)
S.inv <- solve(S)

alpha   <- .05
delta.0 <- c(0,0)

D.T2 <- n * (mus-delta.0) %*% S.inv %*% (mus-delta.0)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p) 

# b

k <- 2

cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF.T1 <- c( mus[1]-cfr.t*sqrt(S[1,1]/n) , mus[1], mus[1]+cfr.t*sqrt(S[1,1]/n) )
IC.BF.T2  <- c( mus[2]-cfr.t*sqrt(S[2,2]/n) , mus[2], mus[2]+cfr.t*sqrt(S[2,2]/n) )

# c

mean1<- (df1$T1+df1$T2)/2
mean2<- (df2$T1+df2$T2)/2

df <- mean1 - mean2

shapiro.test(df)

t.test(df, conf.level = 0.95)
