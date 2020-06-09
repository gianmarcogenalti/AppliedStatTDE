lomba <- read.table("lombardia.txt")
puglia <- read.table("puglia.txt")

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

fit4 <- lm(spesa ~ label + orecchiette + cime, data = df)
summary(fit4)
