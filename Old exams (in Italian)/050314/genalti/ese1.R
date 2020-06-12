library(car)
rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('umea.txt')
head(df)

n <- dim(df)[1]
mcshapiro.test(df)# pval = 0.5152 sono effettivamente normali
V <- cov(df)
mu <- colMeans(df)

alpha <- 0.05
p <- 2
cfr.chisq <- qchisq(1-alpha,p)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

# Characterize the ellipse:
# Axes directions:
eigv <- eigen(V)$vectors
lambda <- eigen(V)$values
# Center:
mu
# Radius of the ellipse:
r <- sqrt(cfr.chisq)
# Length of the semi-axes:
r*sqrt(eigen(V)$values)  

x11()
plot(df, asp = 1, col='gold', pch=19, )
points(mu[1], mu[2], pch = 4, cex = 1.5, lwd = 2)
ellipse(center=mu, shape=V, radius=sqrt(cfr.chisq), col = 'black', lty = 2, center.pch = 4)
dev.off()

IC.T2.DBOD <- c( mu[1]-sqrt(cfr.fisher*V[1,1]/n) , mu[1]+sqrt(cfr.fisher*V[1,1]/n) )
IC.T2.DSS  <- c( mu[2]-sqrt(cfr.fisher*V[2,2]/n) , mu[2]+sqrt(cfr.fisher*V[2,2]/n) )
#########################################################################################

