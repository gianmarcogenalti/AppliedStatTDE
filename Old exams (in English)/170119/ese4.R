df <- read.table("areaC.txt")
head(df)
colnames(df)
attach(df)
mod <- lm(Total ~ Weekend + Petrol + Diesel + Electric + GPL + Natural.Gas + Hybrid.Diesel + Hybrid.Petrol)
coefs <- coef(mod)
# (Intercept)       Weekend        Petrol        Diesel      Electric           GPL   Natural.Gas Hybrid.Diesel 
# 3322.8900301  1421.2316763     1.4434055     0.3917832   -16.5022169     1.7262155     9.3938812     1.6931208 
# Hybrid.Petrol 
# -0.1877149 
beta.00 <- coefs[1]
beta.01 <- coefs[1] + coefs[2]
beta.1  <- coefs[3]
beta.2  <- coefs[4]
beta.3  <- coefs[5]
beta.4  <- coefs[6]
beta.5  <- coefs[7]
beta.6  <- coefs[8]
beta.7  <- coefs[9]
sigma   <- sd(mod$residuals)

summary(mod)
# first of all, only Petrol and Natural.Gas provide a significance with a confidence of over 95%, many variables
# are with high chance not influent
vif(mod)
# Weekend        Petrol        Diesel       Electric        GPL       Natural.Gas   Hybrid.Diesel Hybrid.Petrol 
# 4.805601      2.900463      8.088257      7.146880      6.863037     17.842998      3.833176      6.181106 
# anyways, Natural.Gas has a very high VIF and so is probably collinear with some other regressor. There are many 
# high VIFs too, but not as high as Natural.Gas. We'll try removing it.
par(mfrow = c(2,2))
plot(mod)
shapiro.test(mod$residuals)
# residuals are not considerable normal with a confidence of 95%, moreover we can see a solid leverage effect
detach(df)
pca <- princomp(df[,1:7], scores=TRUE)
summary(pca)
x11()
biplot(pca)
pca$sdev^2/sum(pca$sdev^2)
#Cumulative proportion of explained variance
cumsum(pca$sdev^2)/sum(pca$sdev^2)

loading <- pca$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(loading[,i], ylim = c(-1, 1))

m <- sapply(df[,1:7],mean)
sp1.pc <- pca$scores[,1]
sp2.pc <- pca$scores[,2]

fm.pc1 <- lm(df$Total ~ df$Weekend + sp1.pc + sp2.pc)
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
# il modello è peggiorato

library(car)
A <- rbind(c(0,1,0,0), c(0,0,0,1))
b <- c(0,0)
linearHypothesis(fm.pc1, A, b)

fm.pc2 <- lm(df$Total ~ sp1.pc)
summary(fm.pc2) 
x11()
par(mfrow=c(2,2))
plot(fm.pc2)
shapiro.test(fm.pc2$residuals)

betas2 <- coefficients(fm.pc2)[2]%*%t(pca$loadings[,1])
beta02 <- coefficients(fm.pc2)[1] - betas2%*%m 
beta02
betas2

