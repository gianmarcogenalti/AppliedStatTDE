df <- read.table('snow.txt')


mcshapiro.test(df[df$Snow.G.1=="snow",1:2])
mcshapiro.test(df[df$Snow.G.1!="snow",1:2])

# no normality for low

var(df[df$Snow.G.1=="snow",1:2])
var(df[df$Snow.G.1!="snow",1:2])

plot(df[,1],df[,2],col = df[,3])

library(MASS)

fit <- qda(df[,1:2], df[,3])
fit

pred <- predict(fit, df[,1:2])

plot(df[,1], df[,2], col=df[,3])
points(fit$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(fit, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

s = qda(df[,1:2], df[,3], CV=T) # x, grouping

# Compute the estimate of the AER by leave-out-out cross-validation 
table(class.true=df[,3], class.assignedCV=s$class)

errorCV <- (s$class != df[,3])
errorCV

AERCV   <- sum(errorCV)/length(df[,3])
AERCV

dummy <- rep("snow",length(df[,3]))
errord <- (dummy != df[,3])
errord

AERd   <- sum(errord)/length(df[,3])
AERd

# c

newdata <- data.frame(Temperature.G = -10, Humidity.G = 0.75)
predict(fit,newdata) # -> no-snow
#snow
 