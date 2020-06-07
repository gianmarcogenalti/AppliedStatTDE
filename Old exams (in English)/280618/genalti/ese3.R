library(MASS)
rm(list = ls())
df <- read.table("Precolombian.txt")
head(df)
N <- dim(df)[1]
levels(df$Civilization)
#"Aztec"  "Maya"   "Toltec"
plot(df[,1], df[,2], pch = 16, col = df[,3])
dev.off()
priors = c(0.2, 0.1, 0.7)

mod1 <- lda(Civilization ~ Year + Aspect.Ratio, data = df, priors = priors)
preds1 <- predict(mod1, df)
table(df[,3], preds1$class)
APER1 <- (7 + 6 + 1)/N   #0.08383234

mod2 <- qda(Civilization ~ Year + Aspect.Ratio, data = df, priors = priors)
preds2 <- predict(mod2, df)
table(df[,3], preds2$class)
APER2 <- (4 + 1+ 1+ 3)/N  #0.05389222

# prendo il classificatore quadratico

x11()
plot(df[,1], df[,2], col = factor(preds2$class), pch = 16, xlab = 'Year', ylab = 'Aspect Ratio')
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(Year=x, Aspect.Ratio=y)

z  <- predict(mod2, xy)$post  
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
legend('topleft', levels(df[,3]), pch = 16, col = factor(levels(df[,3])))
points(986, 1.4, pch = 16, cex = 2, col ='orange')
dev.off()

newdat <- data.frame(Year = 986, Aspect.Ratio = 1.4)
guess <- predict(mod2, newdat)
guess$class
# Maya


