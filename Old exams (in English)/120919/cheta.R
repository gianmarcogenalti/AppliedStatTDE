# Clustering and Bonferroni (one way manova)

# Test for independent Gaussian populations
t1.mean <- sapply(vowels[pag=='1',],mean)
t2.mean <- sapply(vowels[pag=='2',],mean)
t1.cov  <-  cov(vowels[pag=='1',])
t2.cov  <-  cov(vowels[pag=='2',])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

# Test: H0: mu.1-mu.2==0 vs H1: mu.1-mu.2!=0
delta.0 <- c(0,0,0,0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P

### question c)
alpha <- 0.1
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC



# Clustering (or Anova) and Bonferroni


fit <- aov(price ~ area)
summary.aov(fit)



# c)
k <- 3
alpha=.01
ng <- table(area)
treat <- levels(area)
N <- dim(data)[1]

Media   <- mean(price)
Mediag  <- tapply(price, area, mean)

dof <- fit$df
Spooled <- sum(fit$residuals^2)/dof


ICrange=NULL
for(i in 1:(b-1)) {
  for(j in (i+1):b) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
  }
}
















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








#  Paired multivariate gaussian


D <- data.frame(a=df[,1]-df[,2], b=df[,3]-df[,4]) 
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

mcshapiro.test(D)


# test for the mean
n <- dim(D)[1]
p <- dim(D)[2]

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .01
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 1%









# qda regions

priors = table(gain)/ n

m = qda(df[,c(1,2)], df[,3])



# discrimination region
plot(df[,1:2], main='Plot', xlab='x1', ylab='x2', pch=20)

points(m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(google=x, apple=y)

z  <- predict(m, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

mcv <- qda(df[,c(1,2)], df[,3], CV=T)
errorsqCV <- (mcv$class != df[,3])

AERqCV   <- sum(errorsqCV)/length(df[,3])
AERqCV


    





# Bonferroni intervals for the mean
df.mean = sapply(df, mean)
df.cov = cov(df)
n = dim(df)[1]
p = dim(df)[2]
alpha = 0.01
# Let's try with Bonferroni intervals ##########4
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = df.mean - cfr.t*sqrt(diag(df.cov)/n),
            center = df.mean, 
            sup = df.mean + cfr.t*sqrt(diag(df.cov)/n))
Bf







# Mean of gaussian test

x.mean   <- sapply(df,mean)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)



### Test on the mean of level alpha=1% ######1
### H0: mu == mu0 vs H1: mu != mu0
### with mu0=c(1,0)
###-----------------------------------
mcshapiro.test(df)

alpha <- 0.99
mu0 <- c(0,0,0)
# T2 Statistics #????
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P







# clustering differences multiple things required

# (c) Identify and comment the four Bonferroni intervals with global confidence
#     90% (lower bound, central value, upper bound) for:
#     - The difference of the mean of the variable length.
#     - The difference of the mean of the variable width.
#     - The difference of the mean of the sum of the variables length and width.
#     - The difference of the mean of the difference of variable length and
#       width.

dm <- (mean1-mean2)
A  <- rbind(c(1,0), c(0,1), c(1,1), c(1,-1))
k  <- dim(A)[1]

A.s2 <- diag(A%*%Sp%*%t(A))
A.dm <- A%*%(mean1-mean2)

Bonf <- cbind(inf=A.dm - qt(1-(alpha/(2*k)), n1+n2-2) * sqrt( A.s2*(1/n1+1/n2) ), 
              center=A.dm, 
              sup=A.dm + qt(1-(alpha/(2*k)), n1+n2-2) * sqrt( A.s2*(1/n1+1/n2) ))
Bonf










# IC for variance of linear model lm
### question c)
k <- 2
alpha <- .1
n <- dim(index)[1]
r <- 3

Z0   <- data.frame(D=1, Anno=1800)
ICBmean <- predict(fitB, Z0, interval='confidence',level=1-alpha/k) 
ICBmean

e <- residuals(fitB)
ICBvar <- data.frame(L=t(e)%*%e/qchisq(1-alpha/(2*k),n-(r+1)),
                     U=t(e)%*%e/qchisq(alpha/(2*k),n-(r+1)))
ICBvar

  



# pca

df = read.table("hotel.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)


# a

pc.df <- princomp(df, scores=T)
pc.df
summary(pc.df)

load.df <- pc.df$loadings


par(mfcol = c(4,2))
for(i in 1:8) barplot(load.df[,i], ylim = c(-1, 1), main=paste("PC",i))

plot(pc.df, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(df,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc.df$sd^2)/sum(pc.df$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))






### question b): Bonferroni method
###           (intervals f  or the components of the mean AND on the variances)

k <- 4
alpha <- 0.1

ICmean <- cbind(inf=x.mean - sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1),
                center= x.mean,
                sup= x.mean + sqrt(diag(x.cov)/n) * qt(1 - alpha/(2*k), n-1))

ICvar <- cbind(inf=diag(x.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=diag(x.cov),
               sup=diag(x.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

ICmean
ICvar







# Two Ways ANOVA IC for differences

# Bonferroni
alpha <- 0.05
g <- 3
b <- 3
p <- 1
n <- 50
N <- n*g*b # 20

DF <- a$df # n*g*b - 1 - (g-1) = 15*3*2-1-2 = 90-3 = 87
Spooled <- sum(a$res^2)/DF #change name pls

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k

qT <- qt(1 - alpha / (2 * k), DF)
# the degrees of freedon of the residuals on the additive model are
# g*b*n-g-b+1

m1  <- mean(index[region=="Canada"])
m2  <- mean(index[region=="Europe"])
m3  <- mean(index[region=="USA"])
ma  <- mean(index[sandwich=="Bacon"])
mb  <- mean(index[sandwich=="Burger"])
mc  <- mean(index[sandwich=="Cheese"])

ic.1 = c(
  inf = m1 - m2 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m1 - m2 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.2 = c(
  inf = m1 - m3 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m1 - m3 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.3 = c(
  inf = m3 - m2 - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = m3 - m2 + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.4 = c(
  inf = ma - mb - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = ma - mb + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.5 = c(
  inf = ma - mc - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = ma - mc + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)
ic.6 = c(
  inf = mc - mb - qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150)),
  sup = mc - mb + qt(1 - alpha / (2 * k), DF)*sqrt(Spooled*(1/150 + 1/150))
)



# Two ways MANOVA IC for DIfferences

fit2<- manova( as.matrix(plastic3) ~ Ex + Ad)
summary.manova(fit2, test="Wilks")

# Both the treatments have a significant effect on the mean (but not
# their interaction, that we could remove)

# Let's verify if this is true for all the variables through appropriate
# conf. int. and tests on the components:

# ANOVA on the components (we look at the 3 axes-directions in R^3
#                          separately)
summary.aov(fit2)

# Bonferroni
alpha <- 0.05
g <- 2
b <- 2
p <- 3
n <- 5
N <- n*g*b # 20

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k

qT <- qt(1 - alpha / (2 * k), g*b*n-g-b+1)
# the degrees of freedon of the residuals on the additive model are
# g*b*n-g-b+1

mExL  <- sapply(plastic3[Ex=='L',],mean)
mExH  <- sapply(plastic3[Ex=='H',],mean)
infEx <- mExH-mExL - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )
supEx <- mExH-mExL + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )

mAdL  <- sapply(plastic3[Ad=='L',],mean)
mAdH  <- sapply(plastic3[Ad=='H',],mean)
infAd <- mAdH-mAdL - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )
supAd <- mAdH-mAdL + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )

IC2   <- list(ExH_ExL=cbind(infEx, supEx), AdH_AdL=cbind(infAd, supAd))
IC2







# ellipses fantastiche e dove trovarle
# por la media

ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)
# centre:
x.mean
# direction of the axes
eigen(x.cov)$vector[,1]
eigen(x.cov)$vector[,2]
# radius
sqrt(cfr.fisher)
# lenght of the semi-axes
sqrt(eigen(x.cov/n)$values)*sqrt(cfr.fisher)













# GEOSTAT


df = read.table("radioville.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
df$DD = 0
df[which(df$D == "U"),"DD"] = 1
attach(df)


coordinates(df) <- c('Long','Lat')
bubble(df,'Bq',do.log=F,key.space='bottom')

v <- variogram(Bq ~ DD, df)
plot(v, main = 'Sample Variogram',pch=19)
v.fit1 = fit.variogram(v, vgm(1, "Sph", 0.5))
plot(v, v.fit1)


# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
df.gstat1 <- gstat(formula = Bq ~ DD,
                   data = df, model=v.fit1)


#b

v.fit2 = fit.variogram(v, vgm(1, "Sph", 0.5, 0.1))
plot(v, v.fit2)


# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
df.gstat2 <- gstat(formula = Bq ~ DD,
                   data = df, model=v.fit2)

#c 


#d

s0.new <- as.data.frame(matrix(c(78.59,35.34,1),1,3))
names(s0.new) <- c('lon','lat','DD')
coordinates(s0.new) <- c('lon','lat')
predict(df.gstat1, s0.new)






# lm max

# question e)
# e) On the basis of the results at point (d), is there statistical 
#    evidence to reject the hypothesis that the maximum of the expected
#    weight (for men and / or women) is reached at the age of 50?

# Deriving and imposing the derivative to be 0 we obtain that the 
# maximum is reached for (beta2<0):
# beta1+2*beta2*eta.max=0, i.e., eta.max=50 <=> beta1+2*beta2*50=0

A <- c(0,1,2*50,0)
b <- 0

linearHypothesis(fit2,A,b)


# question f)
# f) Identify a reduced model of the model (d) that takes into account
#    the statement in paragraph (e) and estimate its parameters.

# Constrained model:
# beta1+2*beta2*50=0  =>  beta1=-100*beta2
fit3 <- lm(peso ~ I(-100*eta+eta^2) + D, data=Fly)
summary(fit3)









# lasso
# Build the matrix of predictors
x <- model.matrix(Sales~ .-Sales, data=df)[,-1]
# Build the vector of response
y <- Sales

fit.lasso <- glmnet(x,y, lambda = 5) # default: alpha=1 -> lasso 
fit.lasso$beta

fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 


# c

# Let's set lambda via cross validation

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- seq(0,100,length=1000)

cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso


plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 
















