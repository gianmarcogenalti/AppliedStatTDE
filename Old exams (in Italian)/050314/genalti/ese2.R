library(MASS)
rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('gender.txt')
head(df)
plot(df[,1],df[,2],col = df[,3])

i.m <- df[,3] == 'M'
i.f <- !i.m

mcshapiro.test(df[i.m,1:2])
mcshapiro.test(df[i.f,1:2])
# sono davanti a dati normali

bartlett.test(df[i.m,1:2], df[i.f,1:2])
# con stessa varianza tra i due gruppi

priors = c(0.5,0.5)
N <- dim(df)[1]
# primo tentativo: lda

mod1 <- lda( Gender~ FL10 + FL20,data = df, prior = priors)
preds1 <- predict(mod1, df[,1:2])
T1 <- table(df[,3], preds1$class)
#   F  M
#F 51  4
#M 18 27
APER1 <- 22/N # 0.22

# secondo tentativo: qda

mod2 <- qda( Gender~ FL10 + FL20,data = df, prior = priors)
preds2 <- predict(mod2, df[,1:2])
T2 <- table(df[,3], preds2$class)
#   F  M
#F 52  3
#M  9 36
APER2<- 12/N # 0.12 scegliamo la qda in quanto ha un aper minore


x11()
plot(df[,1], df[,2], col = factor(preds2$class), pch = 16, xlab = 'FL10', ylab = 'FL20')
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(FL10=x, FL20=y)

z  <- predict(mod2, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')

sum(i.m)# 45
sum(i.f)# 55
# come predittore banale scelgo di classificare tutti come femmine essendo il 55% dei dati
APER.Dummy <- sum(i.m)/N # 0.45 un APER di 0.33 maggiore rispetto a quello del predittore quadratico

newdat <- data.frame(FL10 = 5, FL20 = 7)
guess <- predict(mod2, newdat)
#     F        M
# 0.001457968 0.998542  queste sono le probabilità a posteriori, il nascituro sarà al >99% un maschio!


