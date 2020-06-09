df <- read.table("scotland.txt")

M <- sapply(df,mean)
S <- cov(df)

C <- matrix(c(1, -1, 0, 0, 0,
              1, 0, -1, 0, 0,
              1, 0, 0, -1, 0,
              1, 0, 0, 0, -1), 4, 5, byrow=T)
C

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
delta.0 <- c(0,0,0,0)
q <- 5 
n <- dim(df)[1]

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher

# T2 is much higher than cfr.fisher => the p-value will be very small
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P



C <- matrix(c(1, 0, 0, 0, 0,
              0, 1, 0, 0, 0,
              0, 0, 1, 0, 0,
              0, 0, 0, 1, 0,
              0, 0, 0, 0, 1), 5, 5, byrow=T)
C

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
delta.0 <- c(0,0,0,0)
q <- 5 
n <- dim(df)[1]

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))
 



IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) )
IC.T2


pca <- princomp(scale(df))

# componente 1 -> media delle variabili
# componente 2 -> contee gggggggiovani

