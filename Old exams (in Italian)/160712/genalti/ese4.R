library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
rm(list = ls())
df <- read.table('fluoruro.txt')
head(df)
coordinates(df) <- c('X','Y')
head(df)

plot(variogram(Conc.ppm ~ 1, df),pch=19)
plot(variogram(Conc.ppm ~ D, df),pch=19)
# scegliamo il secondo modello, comprendente il trend perché si stabilizza su un valore
v <- variogram(Conc.ppm ~ D, df)
fit.1 <-fit.variogram(v, vgm(psill =  100, model = "Gau",  range = 0.05))
values.1 <- c(fit.2$psill, fit.2$range)
fit.2 <-fit.variogram(v, vgm(psill =  100, model = "Sph",  range = 0.02))
values.2 <- c(fit.2$psill, fit.2$range)

plot(v,fit.1)
par(new = FALSE)
plot(v,fit.2)
