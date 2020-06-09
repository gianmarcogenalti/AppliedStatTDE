setwd("C:/Users/ffede/OneDrive/Desktop/AppliedStatTDE/Old exams (in English)/170119")
AreaC <- read.table("areaC.txt")

fit <- lm(Total ~ ., data = AreaC)
fit$coefficients
sigma(fit)
plot(fit)
shapiro.test(fit$residuals)
library(car)
vif(fit)

# residui non gaussiani, un briciolo di collinearità

## c

pca <- princomp(AreaC[,-c(8:9)])
#biplot(pca)
# pc1 -> diesel
# pc2 -> benzina
# pc3 -> 
# pc4 -> GPL

pca$sdev^2/sum(pca$sdev^2)
#Cumulative proportion of explained variance
cumsum(pca$sdev^2)/sum(pca$sdev^2)

loading <- pca$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(loading[,i], ylim = c(-1, 1))


sp1.pc <- pca$scores[,1]
sp2.pc <- pca$scores[,2]

fm.pc1 <- lm(Total ~  Weekend + sp1.pc + sp2.pc, data = AreaC)
summary(fm.pc1) 
x11()
par(mfrow = c(2,2))
plot(fm.pc1)
shapiro.test(fm.pc1$residuals)

betas <- coefficients(fm.pc1)[3:4]%*%t(pca$loadings[,1:2])
beta0 <- coefficients(fm.pc1)[1] - betas%*%m
beta0
coefficients(fm.pc1)[2]
betas



