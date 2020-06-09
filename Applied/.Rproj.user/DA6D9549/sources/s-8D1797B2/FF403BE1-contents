lomba <- read.table("lombardia.txt")
puglia <- read.table("puglia.txt")
library(car)

#Spesatotale = Cfissi + Corecchiette  Quantitaorecchiette + Ccime  Quantitacime +  ;

lomba$label <- 1
puglia$label <- 0

df <- rbind(lomba,puglia)

fit1 <- lm(spesa ~ orecchiette + cime, data = lomba)
summary(fit1)

fit2 <- lm(spesa ~ orecchiette + cime, data = puglia)
summary(fit2)

fit3 <- lm(spesa ~ label*orecchiette + label*cime, data = df)
summary(fit3)

vif(fit3)
coef(fit3)

linearHypothesis(fit3, rbind(c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1),
                             c(0,0,0,1,0,0)), c(0,0,0)) 


fit4 <- lm(spesa ~ label + orecchiette + cime, data = df)
summary(fit4)

coef(fit4)

