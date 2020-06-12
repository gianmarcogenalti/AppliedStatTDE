rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('exams.txt')
head(df)

pressure <- df
mcshapiro.test(pressure)

x11()
matplot(t(pressure), type='l')

### question (a)
n <- dim(pressure)[1]
q <- dim(pressure)[2]

M <- sapply(pressure,mean)
M
S <- cov(pressure)
S

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=T)
C
# here we are looking at the effects on the pressure
# after 8, 16 and 24 hours from the instant the drug was
# given

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

P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P
# we accept the null hypothesis that there's no difference

pressure <- df[,3:4]

x11()
matplot(t(pressure), type='l')

### question (a)
n <- dim(pressure)[1]
q <- dim(pressure)[2]

M <- sapply(pressure,mean)
M
S <- cov(pressure)
S

# we build one of the possible contrast matrices to answer
# the question
C <- matrix(c(-1, 1), 1, 2, byrow=T)
C
# here we are looking at the effects on the pressure
# after 8, 16 and 24 hours from the instant the drug was
# given

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .05
delta.0 <- c(0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher

P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P
# we accept the null hypothesis that there's no difference

pca <- princomp(df)
summary(pca)
#                         Comp.1    Comp.2     Comp.3     Comp.4
# Standard deviation     3.9928659 1.6805475 0.55008109 0.49560308
# Proportion of Variance 0.8254012 0.1462168 0.01566567 0.01271638
# Cumulative Proportion  0.8254012 0.9716179 0.98728362 1.00000000

# vediamo come la prima componente spiega molta variabilità

load.tour <- pca$loadings
x11()
par(mfrow = c(2,1))
for(i in 1:2) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))

# la seconda componente principale spiega i voti medi degli ultimi tre appelli, mentra la prima spiega il primo appello
# infatti il primo appello, come si evinceva anche dal matplot, nonostante non abbia differenza in media di voto aveva
# una variabilità di voti molto più alta, e la componente principale che lo rappresenta spiega da sola l'82% della varianza
# mentre gli altri tre appelli avevano pressoché stessa media e varianza


