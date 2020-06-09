#####################################
####      PERMUTATION TESTS      ####
####       (MULTIVARIATE)        ####
#####################################

setwd('/Users/simone/Documents/Polimi/Didattica e divulgazione/Statistica Applicata/2018-19/Permutation Test - Euclidean')

# Example 1: Two (independent) multivariate population test
# Hourly accesses to AreaC: working days vs week-end days

d1 <- read.csv('accessi-orari-areac-2016-09-12-00_00_00.csv', header=T)
d2 <- read.csv('accessi-orari-areac-2016-09-13-00_00_00.csv', header=T)
d3 <- read.csv('accessi-orari-areac-2016-09-14-00_00_00.csv', header=T)
d4 <- read.csv('accessi-orari-areac-2016-09-15-00_00_00.csv', header=T)
d5 <- read.csv('accessi-orari-areac-2016-09-16-00_00_00.csv', header=T)
d6 <- read.csv('accessi-orari-areac-2016-09-17-00_00_00.csv', header=T)
d7 <- read.csv('accessi-orari-areac-2016-09-18-00_00_00.csv', header=T)

week <- rbind(d1[,2], d2[,2], d3[,2], d4[,2], d5[,2], d6[,2], d7[,2])
matplot(seq(0,47)/2,t(week), type='l', col=c(1,1,1,1,1,2,2), lty=1)

t1 <- week[1:5,]
t2 <- week[6:7,]

# Computing a proper test statistic 
# (i.e., squared distance between the two sample mean vectors)
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)

n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n  <- n1 + n2

T20 <- as.numeric((t1.mean-t2.mean) %*% (t1.mean-t2.mean))
T20

# Selecting a proper permutation strategy
# (i.e., data point permutations)

# number of possible data point permutations 
factorial(7)
# number of different values of the test statistic
choose(7,5)

# Estimating the permutational distribution under H0
B <- 100000
T2 <- numeric(B)

for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled <- rbind(t1,t2)
  permutation <- sample(n)
  t_perm <- t_pooled[permutation,]
  t1_perm <- t_perm[1:n1,]
  t2_perm <- t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm <- colMeans(t1_perm)
  t2.mean_perm <- colMeans(t2_perm)
  T2[perm]  <- (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=1000)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val

#################################################################################?
# Example 2: Center of simmetry of one multivariate population
# Relative humidity in Milan during the summer months

hum <- read.csv2('307_Umidita_relativa_2008_2014.csv', header=T)
hum <- hum[,3]
hum <- matrix(hum, ncol=12, byrow=T)[,6:9]

boxplot(hum)
matplot(t(hum), type='l', lty=1)

# center of simmetry under H0
mu0      <- c(65, 65, 65, 65)

# Computing a proper test statistic
# (i.e., squared distance between the sample mean vector and the hypothesized center of simmetry)
x.mean   <- colMeans(hum)
n <- dim(hum)[1]
p <- dim(hum)[2]

T20 <- as.numeric((x.mean-mu0) %*% (x.mean-mu0) )

# Selecting a proper likelihood-invariant strategy
# (i.e., data point reflections)
# We are assuming that under H0 the data distribution is simmetric

# number of possible data point reflections 
2^7

# number of different values of the test statistic 
2^7/2

# Estimating the permutational distribution under H0
B <- 100000 
T2 <- numeric(B) 


for(perm in 1:B){
  # In this case we use changes of signs in place of permutations
  
  # Permuted dataset
  signs.perm <- rbinom(n, 1, 0.5)*2 - 1
  hum_perm <- mu0 + (hum - mu0) * matrix(signs.perm,nrow=n,ncol=p,byrow=FALSE)
  x.mean_perm <- colMeans(hum_perm)
  T2[perm]  <- (x.mean_perm-mu0)  %*% (x.mean_perm-mu0) 
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val


###################################################################
# Example 3: two paired multivariate population test
# (i.e., same days for Milan and Barcelona)
# The data set contains observations of temperature, humidity and wind in Milan and Barcelona
# on 50 different days

t1 <- read.table('barcellona.txt', header=T)
t2 <- read.table('milano.txt', header=T)


library(rgl)
open3d()
plot3d(t1-t2, size=3, col='orange', aspect = F)
points3d(0,0,0, size=6)

p  <- dim(t1)[2]
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n <- n1+n2

# Evaluate the test statistic
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
Spinv   <- solve(Sp)

delta.0 <- c(0,0,0)

diff <- t1-t2
diff.mean <- colMeans(diff)
diff.cov <- cov(diff)
diff.invcov <- solve(diff.cov)

#T20 <- as.numeric(n1 * (diff.mean-delta.0)  %*% (diff.mean-delta.0))
#T20 <- as.numeric(n1 * (diff.mean-delta.0) %*% solve(diag(diag(diff.cov))) %*% (diff.mean-delta.0))
T20 <- as.numeric(n1 * (diff.mean-delta.0) %*% diff.invcov %*% (diff.mean-delta.0))

# With coupled data, we have to reduce the number of permutations
# the only permutations that preserve the likelyhood under H0 are
# permutations within each couple (i.e., exchange Milan and Barcelona within the same day)
# We are assuming the the differences are simmetrically distributed under H0

# number of possible data point reflections 
2^50

# Estimating the permutational distribution under H0
B <- 10000
T2 <- numeric(B)

for(perm in 1:B)
  {
  # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  
  #T2[perm] <- as.numeric(n1 * (diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
  #T2[perm] <- as.numeric(n1 * (diff.mean_perm-delta.0) %*% solve(diag(diag(diff.cov_perm))) %*% (diff.mean_perm-delta.0))
  T2[perm] <- as.numeric(n1 * (diff.mean_perm-delta.0) %*% diff.invcov_perm %*% (diff.mean_perm-delta.0))
  }

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)


# p-value
p_val <- sum(T2>=T20)/B
p_val

2^50
