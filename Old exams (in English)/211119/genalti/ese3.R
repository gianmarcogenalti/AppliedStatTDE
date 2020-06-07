rm(list = ls())
df <- read.table("sansiro.txt")
head(df)

x11()
plot(df[,1],df[,2], col = df[,3], pch = 16)


levels(df$opera)
priors <- c( cancel = .03, regular = .89, suspend = .08)

tool <- lda(opera ~ temperature + precipitation, data = df, prior = priors)
preds <- predict(tool)

l <- table(df$opera, preds$class) # 6 misclassifications 
#            cancel regular suspend
# cancel      14       0       1
# regular      0      15       0
# suspend      0       5      10

APER <- 6/45 # 0.13333

tool <- qda(opera ~ temperature + precipitation, data = df, prior = priors)
preds <- predict(tool)

l <- table(df$opera, preds$class) # 5 misclassifications 
#          cancel regular suspend
#cancel      14       0       1
#regular      0      15       0
#suspend      0       4      11

APER <- 5/45 # 0.1111 

x11()
plot(df[,1], df[,2], col = factor(preds$class), pch = 16, xlab = 'Temp', ylab = 'Prec')
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(temperature=x, precipitation=y)

z  <- predict(tool, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]
z3 <- z[,3] - z[,2]
z4 <- z[,2] - z[,3]
z5 <- z[,1] - z[,3]
z6 <- z[,3] - z[,1]

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')
contour(x, y, matrix(z3, 300), levels=0, drawlabels=F, add=T, lty = 2)  
contour(x, y, matrix(z4, 300), levels=0, drawlabels=F, add=T, lty = 2)
contour(x, y, matrix(z5, 300), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z6, 300), levels=0, drawlabels=F, add=T)

points(28,12, col = 'orange', pch = 16, cex = 2)

newdat <- data.frame(temperature = 28, precipitation = 12)

dev.off()

np <- predict(tool, newdat)
np$class  #suspend

shapiro.test(df[,1])
reg <- df[,3] == 'regular'
sus <- df[,3] == 'suspend'
canc <- df[,3] == 'cancel'

shapiro.test(df[reg,1])
shapiro.test(df[sus,1])
shapiro.test(df[canc,1])
# assumiamo la normalità

var.test(df[reg,1],df[sus,1])
var.test(df[reg,1],df[canc,1])
var.test(df[canc,1],df[sus,1])
# assumiamo le stesse strutture di varianza

ano <- aov(temperature~ opera, data = df)
summary.aov(ano)        
# pvalue = 0.000679 -> relevant



        
        
        