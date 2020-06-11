df <- read.table('montserrat.txt')
library(gstat) 
library(geoR)

#bubble(df,'speed',do.log=F,key.space='bottom')

coordinates(df) <- c('x','y')

svgm <- variogram(speed ~ 1, data = df)
plot(svgm, main = 'Sample Variogram',pch=19)

svgm.anis <- variogram(speed ~ distance, data = df,alpha=c(0,45,90,135))
plot(svgm.anis, main = 'Sample Variogram, distance',pch=19)

svgm.1 <- variogram(speed ~ distance, data = df)
plot(svgm.1, main = 'Sample Variogram, distance',pch=19)

# alpha=c(0,45,90,135) -> sembra giustificata l'isotropia

#scelgo il secondo, il primo non si capisce se abbia un sill o
# cresca indefinitamente


# b

v.fit <- fit.variogram(svgm.1, vgm(psill = 10, "Sph", range = 40))
# if you don't include the nugget, it is omitted
plot(svgm.1, v.fit, pch = 3)
v.fit

v.fit_bad <- fit.variogram(svgm, vgm(psill = 10, "Sph", range = 40))
# if you don't include the nugget, it is omitted
plot(svgm, v.fit_bad, pch = 3)
v.fit_bad


v.anis <- fit.variogram(svgm.anis, vgm(psill = 10, "Sph", range = 40))
plot(svgm.anis, v.anis, pch = 3)
v.anis

#c
gls.badest <- gstat(formula = speed ~ distance, data = df,model = v.fit_bad)

gls.est <- gstat(formula = speed ~ distance, data = df,model = v.fit)
gls.est

# d

D.s0=0
s0= data.frame(x = 402476,y = 4605558, distance = D.s0)
names(s0)=c('x','y','distance')
coordinates(s0)=c('x','y')

predict(gls.est, s0, BLUE = FALSE)
