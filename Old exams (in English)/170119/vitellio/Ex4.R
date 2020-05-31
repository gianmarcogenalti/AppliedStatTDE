rm(list = ls())
data <- read.table("areaC.txt")
head(data)
boxplot(data[,1:7])
data$Weekend <- as.factor(data$Weekend)
fit <- lm(Total ~ ., data = data)
summary(fit)
x11()
par(mfrow=c(2,2))
plot(fit)


# c)
pca <- princomp(data[,1:7], scores=TRUE)
summary(pca)

biplot(pca)
pca$sdev^2/sum(pca$sdev^2)
#Cumulative proportion of explained variance
cumsum(pca$sdev^2)/sum(pca$sdev^2)

loading <- pca$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(loading[,i], ylim = c(-1, 1))


m <- sapply(data[,1:7],mean)

# fitpca <- lm(data$Total ~ data$Weekend + pca$scores)
# summary(fitpca)

# coeffutili <- coefficients(fitpca)[-c(1,2)]
# betas1 <- coeffutili%*%t(pca$loadings)
# norm(betas1 - coefficients(fit)[2:8]) 
                                        # ottimo, si calcolano così
# norm(coefficients(fitpca)[2] - coefficients(fit)[9]) 
                                        # il beta della dummy rimane uguale
# 
# beta01 <- coefficients(fitpca)[1] - betas1%*%m
# norm(beta01-coefficients(fit)[1]) 
                                        # per qualche strano motivo, non devo togliere la dummy

sp1.pc <- pca$scores[,1]
sp2.pc <- pca$scores[,2]

fm.pc1 <- lm(data$Total ~ data$Weekend + sp1.pc + sp2.pc)
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

# d)
library(car)
A <- rbind(c(0,1,0,0), c(0,0,0,1))
b <- c(0,0)
linearHypothesis(fm.pc1, A, b)

fm.pc2 <- lm(data$Total ~ sp1.pc)
summary(fm.pc2) 
x11()
par(mfrow=c(2,2))
plot(fm.pc2)
shapiro.test(fm.pc2$residuals)

betas2 <- coefficients(fm.pc2)[2]%*%t(pca$loadings[,1])
beta02 <- coefficients(fm.pc2)[1] - betas2%*%m 
beta02
betas2

