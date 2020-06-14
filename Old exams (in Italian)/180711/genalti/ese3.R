library(MASS)
rm(list = ls())
df <- read.table('reaction.txt')
head(df)
plot(df[,1],df[,2], col = df[,3], pch = 16)
priors <- c(0.75,0.25)
n <- dim(df)[1]
mod <- qda(reaction~mass + energy, data = df, priors = priors)
preds <- predict(mod, df[,1:2])$class
tab <- table(df[,3], preds)
APER <- (tab[1,2]+tab[2,1])/n

x11()
plot(df[,1], df[,2], col = factor(preds), pch = 16, xlab = 'mass', ylab = 'energy')
x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(mass=x, energy=y)

z  <- predict(mod, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, lty = 6, col = 'blue')

newdat1 <- data.frame(mass = 10, energy = 8)
newdat2 <- data.frame(mass = 12, energy = 10)
newdat3 <- data.frame(mass = 14, energy = 12)

guess1 <- predict(mod, newdat1)
guess1$posterior
guess2 <- predict(mod, newdat2)
guess2$posterior
guess3 <- predict(mod, newdat3)
guess3$posterior



