df <- read.table('sansiro.txt')

plot(df[,1],df[,2], col = df[,3])

priors <- c( cancel = .03, regular = .89, suspend = .08)

mcshapiro.test(df[which(df$opera == 'cancel'),1:2])
mcshapiro.test(df[which(df$opera == 'suspend'),1:2]) # barely gaussian
mcshapiro.test(df[which(df$opera == 'regular'),1:2])


var(df[which(df$opera == 'cancel'),1:2])
var(df[which(df$opera == 'suspend'),1:2]) 
var(df[which(df$opera == 'regular'),1:2])
# sembrano un po' diverse, penso qda vinca

library(MASS)
cl <- qda(opera ~ temperature + precipitation, data = df, prior = priors)
pred <- predict(cl)

# should do cross validation

l <- table(df$opera, pred$class)

errorCV <- (pred$class != df[,3])
errorCV

AERCV   <- sum(errorCV)/length(df[,3])
AERCV
attach(df)


plot(df[,1], df[,2], col=df[,3])
points(cl$means, pch=4,col=c('black','red','green') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=300)
y  <- seq(min(df[,2]), max(df[,2]), length=300)
xy <- expand.grid(temperature=x, precipitation=y)

z  <- predict(cl, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]
z3 <- z[,3] - z[,2]
z4 <- z[,2] - z[,3]
z5 <- z[,1] - z[,3]
z6 <- z[,3] - z[,1]

#contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T, col = 'blue')  
#contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T, col = 'blue')
contour(x, y, matrix(z3, 300), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z4, 300), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z5, 300), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z6, 300), levels=0, drawlabels=F, add=T)

cl$means

# c

newdata <- data.frame(temperature = 28, precipitation = 12)
predict(cl,newdata)

# d

shapiro.test(df[which(df$opera == 'cancel'),1])
shapiro.test(df[which(df$opera == 'suspend'),1]) # barely gaussian
shapiro.test(df[which(df$opera == 'regular'),1])

bartlett.test(df[,1],df[,3]) # homogeneous

anova <- aov(temperature ~ opera)
summary(anova)
# not the best test (we want the opposite relationship)