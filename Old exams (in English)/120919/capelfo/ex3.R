library(car)
library(MASS)
library(class)

mcshapiro.test <- function(X, devstmax = 0.01, sim = ceiling(1/(4*devstmax^2)))
{
  library(mvnormtest)
  n   <- dim(X)[1]
  p   <- dim(X)[2]
  mu  <- rep(0,p)
  sig <- diag(p)
  W   <- NULL
  for(i in 1:sim)
  {
    Xsim <- rmvnorm(n, mu, sig)
    W   <- c(W, mshapiro.test(t(Xsim))$stat)
    # mshapiro.test(X): compute the statistics min(W) for the sample X
  }
  Wmin   <- mshapiro.test(t(X))$stat   # min(W) for the given sample
  pvalue <- sum(W < Wmin)/sim          # proportion of min(W) more extreme than the observed Wmin
  devst  <- sqrt(pvalue*(1-pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, sim = sim)
}


df = read.table("debris.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)
# a

plot(df[,1], df[,2], col=df[,3])

mcshapiro.test(df[risk == "H",c(1,2)])
mcshapiro.test(df[risk == "L",c(1,2)])

image(cov(df[df[,3]=="H",c(1,2)]))
image(cov(df[df[,3]=="L",c(1,2)]))


qda.df <- qda(df[,c(1,2)], risk) # x, grouping



summary(s)
detach(df)

points(qda.df$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda.df, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

qda.df$prior
qda.df$means
qda.df$scaling

# b

s = qda(df[,c(1,2)], risk, CV=T) # x, grouping


# Compute the estimate of the AER by leave-out-out cross-validation 
table(class.true=risk, class.assignedCV=s$class)

errorsqCV <- (s$class != risk)
errorsqCV

AERqCV   <- sum(errorsqCV)/length(risk)
AERqCV


# c

err = rep(100000, 50)
for (k in 1:50) {
  df.knn <- knn.cv(train = df[,c(1,2)], cl = df[,c(3)], k = k)

  errorsqCV <- (df.knn != df[,c(3)])
  err[k]   <- sum(errorsqCV)/length(risk)
}
min(err)

best <- knn.cv(train = df[,c(1,2)], cl = df[,c(3)], k = 27)
plot(df[,1], df[,2], col=1+(df[,3] == "L")+(best != df[,3]))


# d


predict <- knn(train = df[,c(1,2)], test=z, cl = df[,c(3)], k = 27)

