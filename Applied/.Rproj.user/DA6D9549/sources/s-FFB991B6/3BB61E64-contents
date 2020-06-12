#####################################
####      PERMUTATION TESTS      ####
##   TWO INDEPENDENT POPULATIONS   ##
#####################################

rm(list=ls())
setwd('/Users/simone/Documents/Polimi/Didattica e divulgazione/Statistica Applicata/2018-19/Permutation Test - Euclidean')

# We sample n1 data from a first population X1 
# and n2 data froma second population  X2
# Test:
# H0:  X1 =^d  X2
# H1:  X1 !=^d X2

# Case 1. H0 FALSE
# To understand the behaviour of the test, we sample from two populations 
# with different means

# Parameters:
n1 <- n2 <- 10
n <- n1 + n2

# Simulation:
set.seed(240279)
x1 <- runif(n1,0,4)
x2 <- runif(n2,0,4)+3


x_pooled <- c(x1,x2)

par(mfrow=c(1,2))
boxplot(x1,x2,main='Original data')

# How data change if we apply one random permutation?
permutation <- sample(1:n) 

x_perm <- x_pooled[permutation] 
x1_perm <- x_perm[1:n1]
x2_perm <- x_perm[(n1+1):n]

boxplot(x1_perm,x2_perm,main='Permuted data')

abs(mean(x1)-mean(x2))

abs(mean(x1_perm)-mean(x2_perm))
# The mean difference is lower.
# Was that a case?

# TEST
# Test statistic: absolute difference between the two means
T0 <- abs(mean(x1) - mean(x2))
T0

# Cardinality of the permutational space:
factorial(n)

# Number of distinct values of T*:
factorial(n)/(2*factorial(n1)*factorial(n2))

# Minimun achieveable p-value:
1/(factorial(n)/(2*factorial(n1)*factorial(n2)))

# CMC to estimate the p-value
B <- 100000 # Number of permutations
T_stat <- numeric(B) # Vector where we will store the values of T*

# To estimate the p-value we use a loop
# Inside the loop, we do the following:
# 1. choose a random permutation of data (with the command sample)
# 2. calculate and save the test statistic obtained with the permuted data
for(perm in 1:B){
  # permutation:
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # test statistic:
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

# Permutational distribution of T
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

########################################################################################

# Case 2. H0 TRUE
# now we simulate data from two populations with the same distribution

# Simulation:
set.seed(240279)
x1 <- runif(n1,0,4)
x2 <- runif(n2,0,4)+0
x_pooled <- c(x1,x2)

par(mfrow=c(1,2))
boxplot(x1,x2,main='Original data')

# How data change if we apply one random permutation?
permutation <- sample(1:n) 

x_perm <- x_pooled[permutation] 
x1_perm <- x_perm[1:n1]
x2_perm <- x_perm[(n1+1):n]

boxplot(x1_perm,x2_perm,main='Permuted data')

abs(mean(x1)-mean(x2))

abs(mean(x1_perm)-mean(x2_perm))

# TEST
# Test statistic: absolute difference between the two means
T0 <- abs(mean(x1) - mean(x2))
T0

# Cardinality of the permutational space:
factorial(n)

# Number of distinct values of T*:
factorial(n)/(2*factorial(n1)*factorial(n2))

# Minimun achieveable p-value:
1/(factorial(n)/(2*factorial(n1)*factorial(n2)))

# CMC to estimate the p-value
B <- 100000 # Number of permutations
T_stat <- numeric(B) # Vector where we will store the values of T*


for(perm in 1:B){
  # permutation:
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # test statistic:
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

# Permutational distribution of T
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

