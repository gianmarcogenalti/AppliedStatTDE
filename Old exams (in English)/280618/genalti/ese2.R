rm(list = ls())
df <- read.table("Mexican.txt")
head(df)

mod <- aov(price ~ type.food + area + type.food:area, data = df)
summary(mod)
 
#                 Df  Sum Sq Mean Sq F value Pr(>F)    
# type.food        1     746     746   0.097  0.756    
# area             2 6342755 3171378 411.095 <2e-16 ***
# type.food:area   2   25505   12752   1.653  0.193    
# Residuals      294 2268051    7714                
attach(df)

pv = c()
for(i in levels(type.food)){
  pv = c(pv, shapiro.test(price[type.food == i])$p)
}
#pvalues = 1.070913e-05, 2.333344e-05 non posso assumere normali i dati nei sottogruppi dati dalla tipologia
bartlett.test(price ~ type.food)
# p-value = 0.3349 assumo la stessa struttura di covarianza

pv = c()
for(i in levels(area)){
  pv = c(pv, shapiro.test(price[area == i])$p)
}
# pvalues = 0.5822754, 0.8594330, 0.9382871 posso assumere normali i dati in base all'area
bartlett.test(price ~ area)
# p-value = 0.3892 assumo anche la stessa struttura di covarianza

pv = c()
for(i in levels(type.food)){
  for(j in levels(area)){
    pv = c(pv, shapiro.test(price[area == j & type.food == i])$p)
  }
} 
# pval = 0.5355152, 0.8981173, 0.8232974, 0.8374429, 0.8529158, 0.9983219 ho normalità in tutti i sottogruppi

mod <- aov(price ~ area + area:type.food, data = df)
summary(mod)

mod <- aov(price ~ area, data = df)
summary(mod)
#               Df  Sum Sq Mean Sq F value Pr(>F)    
# area          2 6342755 3171378   410.5 <2e-16 ***
# Residuals   297 2294301    7725

m1 <- mean(price[area == 'MexicoCity'])
m2 <- mean(price[area == 'Guanajato'])
m3 <- mean(price[area == 'Cancun'])

DF <- mod$df
Spooled <- sum(mod$res^2)/DF

alpha <- 0.01
k     <- 3
N     <- length(price)
BF    <- rbind(c(m1 -m2 - sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF), 
                 m1 -m2 + sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF)),
               c(m1 -m3 - sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF), 
                 m1 -m3 + sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF)),
               c(m3 -m2 - sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF), 
                 m3 -m2 + sqrt(Spooled / N) * qt(1 - alpha / (2*k), DF))
              )

#99% conf. interval for the mean differences of prices between areas:
# MC - Guanajato = [-19.70089,  10.33089]
# MC - Cancun    = [-325.78159, -295.74981]
# Cancun - Guanajato = [291.06481,  321.09659]

detach(df)
