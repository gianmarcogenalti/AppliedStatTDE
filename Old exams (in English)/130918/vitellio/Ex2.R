rm(list = ls()) 
data = read.table("Waiting.txt")
head(data)

boxplot(waiting ~ course, data=data)
boxplot(waiting ~ city, data=data)
boxplot(waiting ~ city:course, data=data)

fit.aov2.int <- aov(waiting ~ course + city + course:city, data = data)
summary.aov(fit.aov2.int)
attach(data)
Ps <- c(shapiro.test(waiting[course == "Dessert" & city=="Bucarest"])$p,
        shapiro.test(waiting[course == "Main" & city=="Bucarest"])$p,
        shapiro.test(waiting[course == "Starter" & city=="Bucarest"])$p,
        shapiro.test(waiting[course == "Dessert" & city=="Iasi"])$p,
        shapiro.test(waiting[course == "Main" & city=="Iasi"])$p,
        shapiro.test(waiting[course == "Starter" & city=="Iasi"])$p)
Ps

bartlett.test(waiting,course:city)

fit.aov1 <- aov(waiting ~ course, data = data)
summary.aov(fit.aov1)






# c) 
n <- dim(data)[1]
g <- 3
k <- 4
alpha=.05
ng <- table(course)
treat <- levels(course)

Media   <- mean(waiting)
Mediag  <- tapply(waiting, course, mean)

SSres <- sum(residuals(fit.aov1)^2)

S <- SSres/(n-g)

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }
}

DF <- fit.aov1$df # n*g*b - 1 - (g-1)
Spooled <- sum(fit.aov1$res^2)/DF
c(Spooled * DF / qchisq(1 - alpha / (2*k), DF), 
  Spooled * DF / qchisq(alpha / (2*k), DF))

detach(data)
