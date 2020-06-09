library(MASS)
rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('profiling.txt')
head(df)
dim(df)
N <- dim(df)[1]
plot(df[,1],df[,2], col = df[,3], pch = 16)

# assumiamo che le misurazioni siano indipendenti tra gli individui

mcshapiro.test(df[,1:2])# pval = 0.8202862 ok i dati sono normali bivariati
var.test(df[,1],df[,2]) # pval < 2.2e-16 il ratio delle varianze non è uguale a 1, lda richiederebbe questa hp...

mod1 <- lda(type~ t1+t2, data = df)
preds1 <- predict(mod1, df)
table(df[,3], preds1$class)
APER1 <- 80/N # 0.07497657


mod2 <- qda(type~ t1+t2, data = df)
preds2 <- predict(mod2, df)
table(df[,3], preds2$class)
APER2 <- 60/N # 0.05623243

# la qda performa meglio e scelgo quella a maggior ragione dato che rilassa l'ipotesi sulle varianze

mod2$prior
#  resident   tourist 
# 0.7132146 0.2867854

fitted <- preds2$class == 'tourist'
M <- colMeans(df[fitted,1:2])
#    t1       t2 
# 46.44361 15.62778  mean for tourists
M.r <- colMeans(df[-fitted,1:2])
#   t1       t2 
# 37.88309 15.08983  mean for residents
V <- var(df[fitted,1:2])
#        t1         t2
#t1 30.4859024 -0.4178554
#t2 -0.4178554  7.5163713 covariance matrix for tourists
V.r <- var(df[-fitted,1:2])
#         t1        t2
# t1 39.421574 -4.544397
# t2 -4.544397  9.310303 covariance matrix for residents

x11()
plot(df[,1], df[,2], col = factor(preds2$class), pch = 16, xlab = 't1', ylab = 't2', ylim = c(0,30))
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(t1=x, t2=y)

z  <- predict(mod2, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')

points(35,3, col = 'orange', pch = 16, cex = 2)

dev.off()

newdat <- data.frame(t1 = 35, t2 = 3)
guess <- predict(mod2,newdat)$class  #tourist


