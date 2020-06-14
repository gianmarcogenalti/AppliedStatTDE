library(car)
library(glmnet)
library(MASS)
library(class)
library(mvtnorm)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

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


df = read.table("piadeina.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]
attach(df)

m = lm(Sales ~ .-Sales, data=df)

sqrt(var(residuals(m)))


# b

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


