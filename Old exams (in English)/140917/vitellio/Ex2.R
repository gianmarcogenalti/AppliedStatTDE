data <- read.table("olives.txt")

species.name <- factor(data$Restaurant, labels=c('Caffè Muletti','Dalla Luigina'))
iris4        <- data[,1:3]
load("mcshapiro.test.RData")

i1 <- which(species.name=='Caffè Muletti')
i2 <- which(species.name=='Dalla Luigina')
x11()
par(mfrow = c(1, 2))
boxplot(iris4[i1,])
boxplot(iris4[i2,])

n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2

g  <- length(levels(species.name))
p  <- dim(iris4)[2]

### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
Ps <- c(Ps, mcshapiro.test(iris4[i1,])$p)
Ps <- c(Ps, mcshapiro.test(iris4[i2,])$p)
Ps
# 2) same covariance structure (= same covariance matrix Sigma)
S  <-  cov(iris4)
S1 <-  cov(iris4[i1,])
S2 <-  cov(iris4[i2,])



fit <- manova(as.matrix(iris4) ~ species.name)
summary.manova(fit,test="Wilks")






#b)
t1.mean <- sapply(iris4[i1,],mean)
t2.mean <- sapply(iris4[i2,],mean)
t1.cov  <- S1
t2.cov  <- S2
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

alpha   <- .05
delta.0 <- c(0,0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # TRUE: can't reject at 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2.X3 <- c(t1.mean[3]-t2.mean[3]-sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)), t1.mean[3]-t2.mean[3]+sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)) )

IC.T2 <- rbind(IC.T2.X1, IC.T2.X2, IC.T2.X3)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2

