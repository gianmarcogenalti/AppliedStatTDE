rm(list = ls())
data <- read.table("Mexican.txt")
head(data)
attach(data)

x11()
par(mfrow = c(1,3))
boxplot(price ~ type.food, data=data)
boxplot(price ~ area, data=data)
boxplot(price ~ type.food:area, data=data)

Ps <- c(shapiro.test(price[type.food == "Fajitas" & area=="Cancun"])$p,
        shapiro.test(price[type.food == "Fajitas" & area=="Guanajato"])$p,
        shapiro.test(price[type.food == "Fajitas" & area=="MexicoCity"])$p,
        shapiro.test(price[type.food == "Tacos" & area=="Cancun"])$p,
        shapiro.test(price[type.food == "Tacos" & area=="Guanajato"])$p,
        shapiro.test(price[type.food == "Tacos" & area=="MexicoCity"])$p)
Ps

bartlett.test(price,type.food:area)

g <- length(levels(type.food))
b <- length(levels(area))
n <- length(price)/(g*b)

M       <- mean(price)
Mtype   <- tapply(price, type.food, mean)
Marea   <- tapply(price, area, mean)
Mmix    <- tapply(price, type.food:area, mean)


fit2way <- aov(price ~ type.food + area + type.food:area)
summary.aov(fit2way)

fit2wayred <- aov(price ~ type.food + area)
summary.aov(fit2wayred)

fit <- aov(price ~ area)
summary.aov(fit)



# c)
k <- 3
alpha=.01
ng <- table(area)
treat <- levels(area)
N <- dim(data)[1]

Media   <- mean(price)
Mediag  <- tapply(price, area, mean)

dof <- fit$df
Spooled <- sum(fit$residuals^2)/dof


ICrange=NULL
for(i in 1:(b-1)) {
  for(j in (i+1):b) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), dof) * sqrt( Spooled * ( 1/ng[i] + 1/ng[j] )))))
  }
}
