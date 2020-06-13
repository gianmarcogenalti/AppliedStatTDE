df_t <- read.table("tolosa.txt")
df_r <- read.table("roma.txt")
df_m <- read.table("milano.txt")

mcshapiro.test(df_t)
mcshapiro.test(df_r)
mcshapiro.test(df_m)

df_1 <-data.frame(df_r$controlli, df_t$controlli, df_m$controlli)

C1 <- rbind(c(1,-1,0),c(0,1,-1))

delta.0 = c(0,0)
mus <- C1 %*% colMeans(df_1)
Sd <- C1 %*% var(df_1) %*% t(C1)
Sdinv <- solve(Sd)
n <- dim(df_1)[1]
q <- dim(df_1)[2]
alpha <- 0.05

T2 <- n * t( mus - delta.0 ) %*% Sdinv %*% ( mus - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P



df_2 <-data.frame(df_r$multe, df_t$multe, df_m$multe)

C1 <- rbind(c(1,-1,0),c(0,1,-1))

delta.0 = c(0,0)
mus <- C1 %*% colMeans(df_2)
Sd <- C1 %*% var(df_2) %*% t(C1)
Sdinv <- solve(Sd)
n <- dim(df_2)[1]
q <- dim(df_2)[2]
alpha <- 0.05

T2 <- n * t( mus - delta.0 ) %*% Sdinv %*% ( mus - delta.0 )

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher
T2
cfr.fisher
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P

# b

k <- 6
D <- data.frame(rt_c = df_1[,1]-df_1[,2],tm_c = df_1[,2]-df_1[,3],
                rm_c = df_1[,1]-df_1[,3],
                rt_m = df_2[,1] - df_2[,2], tm_m = df_2[,2]-df_2[,3],
                rm_m = df_2[,1]-df_2[,3]) 

boxplot(D)

alpha = 0.1
D.mean <- colMeans(D)
D.cov <- cov(D)
n <- dim(D)[1]
p <- dim(D)[2]

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha/(2*k),p,n-p)
cfr.t <- qt(1-alpha/(2*k),n-1)

c( D.mean-cfr.t*sqrt(diag(D.cov)/n) , D.mean+cfr.t*sqrt(diag(D.cov)/n) )

