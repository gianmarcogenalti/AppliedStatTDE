df <- read.table('horsecolic.txt')

pain <- df[which(df$Pain=="Yes"),-5]
ok <- df[which(df$Pain=="No"),-5]

mcshapiro.test(pain)
mcshapiro.test(ok)
# the horses are gaussian

n1 <- dim(pain)[1]
n2 <- dim(ok)[1]
n <- dim(df)[1]
p <- dim(pain)[2]

S1 <- cov(pain)
S2 <- cov(ok)
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))

Spooled <- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)


alpha <- 0.01
k <- 4

x.mean1 <- sapply(pain, mean)
x.mean2 <- sapply(ok, mean)
x.mean <- x.mean1 - x.mean2


cfr.t <- qt(1-alpha/(2*k), n1+n2-2)

Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(Spooled)*(1/n1+1/n2)),
            center = x.mean,
            sup = x.mean + cfr.t*sqrt(diag(Spooled)*(1/n1+1/n2)))
Bf

# sembra che ci sia differenza nelle pulsazioni e nel rate respiratorio

# b

plot(df[,2:3], col = df[,5])

library(MASS)
cl <- lda(df[,2:3], df[,5])
cl2 <-  qda(df[,2:3], df[,5])

iris2 <- df[,c(2:3,5)]

plot(iris2[,1:2], pch=20, col = iris2[,3] )

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(Pulse=x, Respiratory.rate=y)

z  <- predict(cl, xy)$post
z1 <- z[,1] - z[,2]
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,
        col = 'green')  

z_  <- predict(cl2, xy)$post
z2 <- z_[,1] - z_[,2]
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,
        col = 'pink')  

# c

reclass <- predict(cl2, df[,2:3])
table(class.true=df$Pain, class.assigned=reclass$class)

errori <- (reclass$class != df$Pain)
length(errori)

APER   <- sum(errori)/n
APER   # 0.06 lda / 0.0533 qda 

cl3 <- glm(Pain ~ Pulse + Respiratory.rate , data = df, family = 'binomial')

reclass <- predict(cl3, df[,2:3])
reclass <- exp(reclass)/(1+exp(reclass))
reclass <- ifelse(reclass>0.5,1,0)
errori <- (reclass != (as.numeric(df$Pain)-1))
length(errori)
APER   <- sum(errori)/n
APER  # logistica 0.0533
# 0.06 lda / 0.0533 qda 


plot(iris2[,1:2], pch=20, col = iris2[,3] )

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(Pulse=x, Respiratory.rate=y)

z  <- predict(cl3, xy)
z <- 1/(1+exp(-z)) # non cambia nulla 1/ o exp/
z <- (z - 0.5)*2
#z1 <- z[,1] - z[,2]
contour(x, y, matrix(z, 200), levels=0, drawlabels=F, add=T,
        col = 'blue') 


# lda e logistica hanno una frontiera molto simile lineare
# la logistica ha il vantaggio di poter muovere la frontiera
# cambiando la probabilità di cutoff comodamente

# dai mettiamoci pure un knn

