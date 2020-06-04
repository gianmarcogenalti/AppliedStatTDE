df <- read.table("revenues.txt")
head(df)

load("mcshapiro.test.RData")

mcshapiro.test(df) # i dati sono normali multivariati

x11()
matplot(t(df), type = 'l')

### question (a)
n <- dim(df)[1]
q <- dim(df)[2]

M <- sapply(df,mean)
M
S <- cov(df)
S

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=T)
C


# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .10
delta.0 <- c(0,0,0)

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

# c'è assolutamente differenza nella revenue lungo il tempo

# Simultaneous T2 intervals
IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) )
IC.T2

# Bonferroni intervals 
k     <- q - 1   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) )
IC.BF

# a settembre c'è un crollo della revenue dovuto probabilmente alla fine della stagione balneare, che ha il suo
# picco ad agosto, il mese per eccellenza per le vacanze al mare
