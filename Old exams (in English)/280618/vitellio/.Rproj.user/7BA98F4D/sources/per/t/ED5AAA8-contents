rm(list = ls())
data <- read.table("Hotels.txt")
head(data)

pos <- ifelse(data$Position == "Seafront", 1, 0)
season <- ifelse (data$Season == "WetSeason", 1, 0)

data$dummy <- rep(0, dim(data)[1])
for(i in 1:dim(data)[1])
{
  if(pos[i] == 0 && season[i] == 0) data$dummy[i] = 1
  if(pos[i] == 1 && season[i] == 0) data$dummy[i] = 2
  if(pos[i] == 0 && season[i] == 1) data$dummy[i] = 3
  if(pos[i] == 1 && season[i] == 1) data$dummy[i] = 4
}
data$dummy <- as.factor(data$dummy)  
fit <- lm(price ~ dummy + dummy*I(1 + cos(4*pi/365 * t)), data=data)
summary(fit)

coef(fit)[1]
coef(fit)[1]+coef(fit)[2]
coef(fit)[1]+coef(fit)[3]
coef(fit)[1]+coef(fit)[4]
coef(fit)[5]
coef(fit)[5]+coef(fit)[6]
coef(fit)[5]+coef(fit)[7]
coef(fit)[5]+coef(fit)[8]

x11()
par(mfrow=c(2,2))
plot(fit)

shapiro.test(residuals(fit))

# b)
library(car)
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0,0,0),
                       c(0,1,0,-1,0,0,0,0),
                       c(0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,1,0,-1)),
                 c(0,0,0,0))

linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0,0,0),
                       c(0,0,1,-1,0,0,0,0),
                       c(0,0,0,0,0,1,0,0),
                       c(0,0,0,0,0,0,1,-1)),
                 c(0,0,0,0))

linearHypothesis(fit,
                 rbind(c(0,0,0,0,0,1,0,0),
                       c(0,0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,0,1)),
                 c(0,0,0))


# c)

fitre <- lm(price ~ dummy + I(1 + cos(4*pi/365 * t)), data=data)
summary(fitre)


# d)
alpha <- 0.01
t0 <- 0
newdatum <- data.frame(t=t0, dummy=as.factor(2))
predict(fitre, newdata = newdatum, interval = "confidence", level = 1 - alpha)



# Z <- model.matrix(fitre)
# 
# s <- summary(fitre)$sigma
# 
# n <- dim(data)[1]
# r <- 4
# betas <- coefficients(fitre)
# a <- c(1,1,0,0,0)
# invz <- solve(t(Z)%*%Z)
# IC <- rbind(
#   inf = a%*%betas - s*sqrt(t(a)%*%invz%*%a)*sqrt((r+1)*qf(1-alpha,r+1,n-(r+1))),
#   fit = a%*%betas,
#   sup = a%*%betas + s*sqrt(t(a)%*%invz%*%a)*sqrt((r+1)*qf(1-alpha,r+1,n-(r+1)))
# )
# IC
# 
# 
# i1 <- which(data$dummy == 2)
# summary(data[i1,]$price)
