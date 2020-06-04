load("mcshapiro.test.RData")
df <- read.table("sunchair.txt")
head(df)

matplot(t(df), type = 'l')
mcshapiro.test(df) # normali multivariati

### question (a)
n <- dim(df)[1]
q <- dim(df)[2]

M <- sapply(df,mean)
S <- cov(df)

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=T)

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
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

# there's statistical evidence to say that the prices are not the same through the year

### question (b)

# Bonferroni intervals 
k     <- q  # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF <- cbind( M - cfr.t*sqrt(diag(S)/n) , M, M + cfr.t*sqrt(diag(S)/n) )
IC.BF
#                  Mean prices         
# Mar.May  41.49724 42.60177 43.70631
# Jun.July 49.60182 50.44403 51.28625
# Aug.Oct  44.08892 44.69871 45.30850
# Nov.Feb  37.64584 38.66500 39.68416

# there's no intersection between any subsequent interval! prices vary

