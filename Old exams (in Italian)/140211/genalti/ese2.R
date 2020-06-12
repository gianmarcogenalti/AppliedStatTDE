library(MASS)
rm(list = ls())
f <- read.table('false.txt')
t <- read.table('true.txt')
head(f)
head(t)
df <- rbind(t,f)
df$fake <- rep(NA,200)
df[1:100,3] <- 0
df[101:200,3] <- 1
head(df)
tail(df)
priors <- c(0.95, 0.05, priors = priors)

plot(df[,1],df[,2], col = factor(df[,3]), pch = 16)

mod <- qda(fake ~ Lunghezza + Larghezza, data = df)
preds <- predict(mod, df[,1:2])
tab <- table(df[,3], preds$class)
#    0  1
# 0 94  6
# 1 19 81
APER <- 25/200# APER = 0.125 buon valore, contro lo 0.5 del dummy predictor

x11()
plot(df[,1],df[,2], col = preds$class,  pch = 16)
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(Lunghezza=x, Larghezza=y)

z  <- predict(mod, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')
points(85.5, 55, col = 'orange', pch = 16, cex = 2)

newdat <- data.frame(Lunghezza = 85.5, Larghezza = 55)
guess <- predict(mod, newdat)
# 0, il biglietto sarà autentico

