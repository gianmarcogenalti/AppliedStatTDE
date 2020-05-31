data <- read.table("sunchair.txt")
head(data)
load("mcshapiro.test.RData")
pressure <- data
mcshapiro.test(pressure)
n <- dim(pressure)[1]
q <- dim(pressure)[2]
S <- cov(pressure)
M <- sapply(pressure,mean)
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=TRUE)
# Test: H0: C*mu=0 vs H1: C*mu!=0
delta.0 <- c(0,0,0)
Md <- C %*% M
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)
alpha <- .05
T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))





## punto b

alpha <- .05
k <- q # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = M - cfr.t*sqrt(diag(S)/n),
            center = M,
            sup = M + cfr.t*sqrt(diag(S)/n))
Bf
