df <- read.table('flight.txt')

n <- dim(df)[1]

df$tot <- df$Linate+df$Malpensa+df$Orio
shapiro.test(df$tot)
# è normale 

mu <- mean(df$tot)
var_ <- var(df$tot)
alpha <- 0.1
k<- 2

IC_mu <- c(mu - qt(1-alpha/(2*k),n-1)*sqrt(var_/n), mu + qt(1-alpha/(2*k),n-1)*sqrt(var_/n))
IC_mu

IC_var <- c((n-1)*var_/qchisq(1-alpha/(2*k),n-1),(n-1)*var_/qchisq(alpha/(2*k),n-1))
IC_var

# b

C = rbind(c(1,-1,0),c(1,0,-1))
mus <- C %*% colMeans(df[,1:3])
S <- var(df[,1:3])
q <- 3

delta.0 <- c(0,0)

Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( mus - delta.0 ) %*% Sdinv %*% ( mus - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher

# T2 is much higher than cfr.fisher => the p-value will be very small
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P

# P value 0 -> rifiuto in ogni caso

# c

C = rbind(c(-2,1,0),c(0,1,-2))
mus <- C %*% colMeans(df[,1:3])
S <- var(df[,1:3])
q <- 3

delta.0 <- c(0,0)

Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( mus - delta.0 ) %*% Sdinv %*% ( mus - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher

# T2 is much higher than cfr.fisher => the p-value will be very small
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P

# P value 0.32 -> bon