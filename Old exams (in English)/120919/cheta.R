# Cluster and difference of means
### question c)
alpha <- 0.05
k <- 6
k

qT <- qt(1-alpha/(2*k), N-g)

# I need the diagonal of W
fit$res   # residuals of the estimated model
W <- diag(t(fit$res) %*% fit$res)/(N-g)   
W
# mean within the groups
nuovo=cbind(satellite,sata)
m1 <- colMeans(nuovo[which(nuovo$sata==1),1:2])
m2 <- colMeans(nuovo[which(nuovo$sata==2),1:2])
m3 <- colMeans(nuovo[which(nuovo$sata==3),1:2])
m1
m2
m3


Bf12 <- cbind(m1-m2 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[1]+1/ng[2])*W), m1-m2, m1-m2 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[1]+1/ng[2])*W))
Bf23 <- cbind(m2-m3 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[2]+1/ng[3])*W), m2-m3, m2-m3 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[2]+1/ng[3])*W))
Bf31 <- cbind(m3-m1 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[3]+1/ng[1])*W), m3-m1, m3-m1 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[3]+1/ng[1])*W))

IC <- list(gemme1-gemme2=Bf12, gemme2-gemme3=Bf23, gemme3-gemme1=Bf31)
IC











#_______________________________________________________________________________
##### Test for the mean of two independent Gaussian populations ###########6
#####------------------------------------------------------------

# we build the data
t1 <- matrix(c(3,3,1,6,2,3),2)
t1 <- data.frame(t(t1))
t2 <- matrix(c(2,3,5,1,3,1,2,3),2)
t2 <- data.frame(t(t2))

t1
t2

n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] # p=2


# we compute the sample mean, covariance matrices and the matrix
# Spooled

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .01
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# P-value high (we don't reject at 1%,5%,10%)

# Simultaneous T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2








# 


