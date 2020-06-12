rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('oil.txt')
head(df)

matplot(t(df),type = 'l')
points(1:5, sapply(df, mean), pch = 16)

pressure <- df

mcshapiro.test(pressure)

### question (a)
n <- dim(pressure)[1]
q <- dim(pressure)[2]

M <- sapply(pressure,mean)
M
S <- cov(pressure)
S

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1,  1,  0, 0,0,
               0, -1,  1, 0,0,
               0,  0, -1, 1,0,
               0,  0,  0,-1,1), 4, 5, byrow=T)
C
# here we are looking at the effects on the pressure
# after 8, 16 and 24 hours from the instant the drug was
# given

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
delta.0 <- c(0,0,0,0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher

# T2 is  higher than cfr.fisher => the p-value will be very small
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P # the p-value is small and let's accept the alternative hypothesis, there's a difference

k     <- q - 1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) )
#     inf        mean      upp
#  0.8825702  4.3006154 7.718661
# -1.6580833  0.6864615 3.031006
# -2.9853227 -0.4236923 2.137938
# -1.0795106  1.6035385 4.286588

# vediamo infatti come il primo intervallo non contenga lo 0 e abbia come upper bound 7.718661, questo significa che 
# lì la differenza non è nulla

diff <- -df[,1]+df[,2]
shapiro.test(diff) # normale

t.test(diff, mu = 5)
# p-value = 0.5594 possiamo confermare questa ipotesi







