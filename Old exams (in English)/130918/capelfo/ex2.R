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


df = read.table("Waiting.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

attach(df)

# a

levels(course)
shapiro.test(waiting[course == "Starter"])
shapiro.test(waiting[course == "Main"])
shapiro.test(waiting[course == "Dessert"])

levels(city)
shapiro.test(waiting[city == "Iasi"])
shapiro.test(waiting[city == "Bucarest"])

a = aov(waiting ~ course + city + course:city)
summary(a)

# b
b = aov(waiting ~ course)
summary(b)

bartlett.test(waiting, course)


# c

n       <- length(course)      # total number of obs.
ng      <- table(course)       # number of obs. in each group
z   <- levels(course)      # levels of the treatment
g       <- length(z)     # number of levels (i.e., of groups)
p = 1

k <- p*g*(g-1)/2 + 1
alpha= 0.05

Mediag  <- tapply(waiting, course, mean)
SSres <- sum(residuals(b)^2)
S <- SSres/(n-g)

# Example: CI for the difference "casein - horsebean"


for(i in 1:3) {
  for(z in (i):3) {
  print(paste(i, "-", z))
  print(as.numeric(c(Mediag[i]-Mediag[z] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[z] )),
               Mediag[i]-Mediag[z] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[z] )))))
  }
}

DF <- b$df # n*g*b - 1 - (g-1) = 15*3*2-1-2 = 90-3 = 87 #???????????
Spooled <- sum(b$res^2)/DF


BF    <- c(Spooled * DF / qchisq(1 - alpha / (2*k), DF), 
                 Spooled * DF / qchisq(alpha / (2*k), DF))
BF










