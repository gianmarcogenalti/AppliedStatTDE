library(car)
rm(list = ls())
pu <- read.table('puglia.txt')
lo <- read.table('lombardia.txt')
head(lo)
head(pu)
pu$region <- 'puglia'
lo$region <- 'lombardia'
df <- rbind(pu,lo)
tail(df)

mod1 <- lm(spesa~orecchiette + cime + orecchiette:region + cime:region + region, data = df) 
summary(mod1) # l'unica cosa che non sembra pesare nel modello è l'interazzione tra orecchiette e regione

vif(mod1) # c'è forte collinearità nel modello
coef(mod1)

linearHypothesis(mod1, rbind(c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1),
                             c(0,0,0,1,0,0)), c(0,0,0)) # pval = 6.434e-11 la regione sembra influire sul costo totale

linearHypothesis(mod1, c(0,0,0,0,0,1), 0) # pval = 0.4198 la regione non influenza in particolare il costo delle cime di rapa
linearHypothesis(mod1, c(0,0,0,0,1,0), 0) # pval = 0.498 mentre non sembra influenzare nemmeno il costo delle orecchiette
linearHypothesis(mod1, c(0,0,0,1,0,0), 0) # pval = 0.0008996 i costi fissi tuttavia sembrano influenzati (logico)

mod2 <- lm(spesa~orecchiette + cime + region, data = df) 
summary(mod2) # l'R^2 è rimasto costante ma tutti i regressori sembrano significativi adesso
vif(mod2) # abbiamo ridotto la collinearità
coef(mod2)
