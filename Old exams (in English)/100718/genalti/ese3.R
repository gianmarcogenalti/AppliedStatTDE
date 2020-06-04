library(car)

df1 <- read.table("presales.txt")
head(df1)
df2 <- read.table("sales.txt")
head(df2)

load("mcshapiro.test.Rdata")
mcshapiro.test(df1)
mcshapiro.test(df2)
# siamo davanti a dati normali bivariati

D <- data.frame(ff = (df1$Flip.Flops- df2$Flip.Flops)/df1$Flip.Flops, ss = (df1$Swimsuit - df2$Swimsuit)/df1$Swimsuit)
head(D)
mcshapiro.test(D) # è sempre normale bivariata
matplot(t(D), type = 'l') # proviamo a dimostrare con un test che la differenza è nulla

n <- dim(D)[1]  # 45
p <- dim(D)[2]  #  2

D.mean   <- sapply(D,mean) 
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .01
delta.0 <- c(0.2,0.2)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # TRUE: we accept H0 at level 1%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P # 0.1749

# possiamo accettare l'ipotesi che lo sconto sia del 20% su entrambi i prodotti

D.mean   <- sapply(D,mean) 
D.cov    <- cov(D)
D.mean # point estimate for the mean of discount
D.cov # point estimate for covariance matrix of discount

# Ellipsoidal confidence region with confidence level 99%

### Simultanouse T2 intervals
IC.T2.ff <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.ss <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )

T2 <- rbind(IC.T2.ff, IC.T2.ss)
dimnames(T2)[[2]] <- c('inf','center','sup')
T2


x11()
plot(D, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)
abline(v = T2[1,1], col='red', lwd=1, lty=2)
abline(v = T2[1,3], col='red', lwd=1, lty=2)
abline(h = T2[2,1], col='red', lwd=1, lty=2)
abline(h = T2[2,3], col='red', lwd=1, lty=2)
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')

# worst direction

worst <- D.invcov %*% (D.mean-delta.0)
worst <- worst/sqrt(sum(worst^2))
worst
# Angle with the x-axis:
theta.worst <- atan(worst[2]/worst[1])+pi
theta.worst

# Confidence interval along the worst direction:
IC.worst  <- c( D.mean %*% worst - sqrt(cfr.fisher*(t(worst)%*%D.cov%*%worst)/n),
                D.mean %*% worst,
                D.mean %*% worst + sqrt(cfr.fisher*(t(worst)%*%D.cov%*%worst)/n) )
IC.worst
delta.0%*%worst

# Extremes of IC.worst in the coordinate system (x,y):
x.min <- IC.worst[1]*worst
x.max <- IC.worst[3]*worst
m1.ort <- -worst[1]/worst[2]
q.min.ort <- x.min[2] - m1.ort*x.min[1]
q.max.ort <- x.max[2] - m1.ort*x.max[1]
abline(q.min.ort, m1.ort, col='forestgreen', lty=2,lwd=1)
abline(q.max.ort, m1.ort, col='forestgreen', lty=2,lwd=1)

m1=worst[2]/worst[1] # worst direction
abline(0, m1, col='grey35')
segments(x.min[1],x.min[2],x.max[1],x.max[2],lty=1,lwd=2, col='forestgreen')



