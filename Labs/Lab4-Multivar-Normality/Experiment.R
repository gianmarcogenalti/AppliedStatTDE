# Experiment: we repeat 200 times the sample generation (under H0) and the univariate tests along "all" 
# (i.e., a fine grid of) the directions. We count how many univariate rejections and how 
# many multivariate rejections we get 

library(mvtnorm)

mu <- c(1,2)
sig <- matrix(c(1,1,1,2), 2)
n   <-  150

theta   <- seq(0, pi - pi/180, by = pi/180)

Tot <- 0
Sin <- rep(0, 180)

x11()
plot(theta, rep(0, 180), pch = '', ylim = c(0,200), xlim = c(0, pi), ylab='number of rejections',
     main = 'Univariate vs multivariate level')
abline(h = 20, lty = 2)
abline(h = 0, lty = 1)
points(theta, Sin, col = 'blue', pch = 16)
points(pi, Tot, col = 'red', pch = 16)

for(i in 1:200)
{
  
  Y <- rmvnorm(n, mu, sig)
  
  theta   <- seq(0, pi - pi/180, by = pi/180)
  W       <- NULL
  P       <- NULL
  for(i in 1:length(theta))
  {
    a   <- c(cos(theta[i]), sin(theta[i]))
    w   <- shapiro.test(Y %*% a)$statistic
    p   <- shapiro.test(Y %*% a)$p.value
    W   <- c(W, w)
    P   <- c(P, p)
  }
  
  Sin <- Sin + as.numeric(P < (0.10))
  Tot <- Tot + (sum(P < (0.10)) > 0)
  
  points(theta, Sin, col = 'blue', pch = 16)
  points(pi, Tot, col = 'red', pch = 16)
  
}

# To globally reject at 10% we should
# (a) decrease the level of the univariate tests (e.g., Bonferroni corrections)
# or
# (b) think multivariate
