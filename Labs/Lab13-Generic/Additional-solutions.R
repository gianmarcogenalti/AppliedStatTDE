
setwd("D:/RTDA/Didattica/Applied Statistics MATE 19-20/Lab 13 - 05062020")
load("D:/RTDA/Didattica/Applied Statistics MATE 19-20/Lab 5 - 16042020/mcshapiro.test.RData")

# Problem 1 of 13.09.12
# Nel file library.txt sono riportate per le 23 biblioteche comunali milanesi
# le spese (riferite all'anno 2011) relative all'acquisto libri, alla retribuzione
# del personale, al consumo di energia elettrica e infine ad altri costi. Assumendo
# iid normali i dati relativi alle 23 biblioteche:
# a) si forniscano 5 intervalli di confidenza globale 90% per la media delle quattro
# voci di spesa e per la loro somma;
# b) si forniscano 5 intervalli di confidenza globale 90% per la deviazione standard
# delle quattro voci di spesa e per la loro somma;
# c) si confermi/smentisca l'ipotesi secondo la quale la spesa media in libri copre
# la metà della spesa totale.


library=read.table('library.txt', header=T)
head(library)

# a) si forniscano 5 intervalli di confidenza globale 90% per la media delle quattro
# voci di spesa e per la loro somma;
mcshapiro.test(library)

n <- dim(library)[1]  
p <- dim(library)[2]  

x.mean   <- sapply(library,mean)
x.cov    <-  cov(library)

alpha   <- .1
k <- 5 
cfr.t <- qt(1-alpha/(2*k),n-1)
A=rbind(diag(rep(1,4)),
        c(1,1,1,1))


IC.BF <- cbind( A%*%x.mean-cfr.t*sqrt(diag(A%*%x.cov%*%t(A))/n) , 
                A%*%x.mean,
                A%*%x.mean+cfr.t*sqrt(diag(A%*%x.cov%*%t(A))/n) )

colnames(IC.BF)=c('Inf','Center', 'Sup')
IC.BF

# b) si forniscano 5 intervalli di confidenza globale 90% per la deviazione standard
# delle quattro voci di spesa e per la loro somma;
k <- 5
qCinf <- qchisq(1 - alpha / (2*k), n-1)
qCsup <- qchisq(alpha / (2*k), n-1)

BF    <- cbind(inf=diag(A%*%x.cov%*%t(A)) * (n-1) / qCinf,
               center=diag(A%*%x.cov%*%t(A)),
               sup=diag(A%*%x.cov%*%t(A)) * (n-1) / qCsup)
sqrt(BF)

# c) si confermi/smentisca l'ipotesi secondo la quale la spesa media in libri copre
# la metà della spesa totale.
A=cbind( .5,-.5,-.5,-.5)
lcomb=as.matrix(library)%*%t(A)
t.test(x = lcomb, conf.level = 1-.1)

cfr.t <- qt(1-alpha/2,n-1)
IC.BF <- cbind( A%*%x.mean-cfr.t*sqrt(diag(A%*%x.cov%*%t(A))/n) , 
                A%*%x.mean,
                A%*%x.mean+cfr.t*sqrt(diag(A%*%x.cov%*%t(A))/n) )
IC.BF

#_______________________________________________________________________________
# Problem 4 of 29.06.10
# Nel file energy.txt sono riportati i consumi elettrici istantanei [MW] allo 
# scoccare di ogni ora nella citta' di Belfast, per il mese di Giugno. Assumendo
# le misurazioni indipendenti e normalmente distribuite con pari varianza e media
# mu = A.g +B.g*[1-cos(2pi/24*t)] dipendente dall'ora di misurazione t 
# (t = 0, 1, 2, ... , 23) e dalla natura infrasettimanale o meno del giorno di
# misurazione (g in {"Lun-Ven", "Sab-Dom"}).
# a) Si stimino i parametri del modello.
# b) Si fornisca, se possibile, un opportuno modello ridotto, se ne giustifichi
#    la scelta e se ne interpretino i parametri.
# c) Sulla base del modello ridotto (b), si forniscano degli intervalli di 
#    confidenza globale 90% per la media e la varianza dei consumi raggiunti
#    in data odierna (martedi' 29 giugno 2010) alle ore 12:00 ed alle ore 18:00.
# d) Sulla base del modello ridotto (b), vi e' evidenza che di venerdi' il consumo
#    medio massimo sia piu' del doppio del consumo medio minimo?
# e) Sulla base del modello ridotto (b), vi e' evidenza che di sabato il consumo
#    medio massimo sia piu' del doppio del consumo medio minimo?

energy=read.table('energy.txt', header = T, sep='\ ')
head(energy)

# Build the data matrix
t = rep(0:23,28)
y=NULL
for(i in 1:dim(energy)[1])
  y=c(y,as.numeric(energy[i,-1]))
tmp = rep(energy$giorno.della.settimana, each=(dim(energy)[2]-1))
D=ifelse(tmp %in% c('sabato', 'domenica'), 0, 1)

data = data.frame(y,t,D)

# Estimate the model parameters
fit = lm(y ~ D + I(1-cos(2*pi/24*t)) + I(D*(1-cos(2*pi/24*t))), data = data)
summary(fit)

# Reduce the model
shapiro.test(residuals(fit))
par(mfrow=c(2,2))
plot(fit)

A=rbind(c(0,1,0,0),
        c(0,0,0,1))
b=c(0,0)

linearHypothesis(fit, A, b)

# Reduced model
fit2 = lm(y ~ I(1-cos(2*pi/24*t)) + I(D*(1-cos(2*pi/24*t))), data = data)
summary(fit2)
shapiro.test(residuals(fit2))
par(mfrow=c(2,2))
plot(fit2)

# Confidence intervals
k=3
alpha=.1
new.data=data.frame(t=c(12,18), D=1)
predict(fit2, new.data, level = 1-alpha/k, interval = 'confidence')

sigma2=sum(residuals(fit2)^2)/669

qCinf <- qchisq(1 - alpha / (2*k), 669)
qCsup <- qchisq(alpha / (2*k), 669)

BF    <- cbind(inf=sigma2 * 669 / qCinf,
               center=sigma2 ,
               sup=sigma2 * 669 / qCsup)
BF

# question d)
# Having derived A+(B+C)*[1-cos(2pi/24*t)] we obtain t=0 and t=12, with 
A=rbind(c(1,0,0),
        c(1,2,2))
A%*%as.vector(coefficients(fit2))
#t=0 is min, t=12 is max

# Test H0: 2(B+C)-A<0 vs H1: 2(B+C)-A>0
A=t(as.vector(c(-1,2,2)))
A%*%as.vector(coefficients(fit2))
A%*%vcov(fit2)%*%t(A)
n=dim(data)[1]

1-pt(A%*%as.vector(coefficients(fit2))/sqrt(A%*%vcov(fit2)%*%t(A)), n-3)
# Reject H0

# question e)
# Having derived A+B*[1-cos(2pi/24*t)] we obtain t=0 and t=12, with 
A=rbind(c(1,0,0),
        c(1,2,0))
A%*%as.vector(coefficients(fit2))
#t=0 is min, t=12 is max

# Test H0: 2B-A<0 vs H1: 2B-A>0
A=t(as.vector(c(-1,2,0)))
A%*%as.vector(coefficients(fit2))
A%*%vcov(fit2)%*%t(A)
n=dim(data)[1]

1-pt(A%*%as.vector(coefficients(fit2))/sqrt(A%*%vcov(fit2)%*%t(A)), n-3)
# Reject H0

