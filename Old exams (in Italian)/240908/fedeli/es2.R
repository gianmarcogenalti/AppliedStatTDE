df <- read.table('luggage.txt')

mcshapiro.test(df[which(df$Classe == "H"),1:2])
mcshapiro.test(df[which(df$Classe == "L"),1:2])
# squisitamente normali
var(df[which(df$Classe == "H"),1:2])
var(df[which(df$Classe == "L"),1:2])
# le varianze sembrano uguali 

shapiro.test(df[which(df$Classe == "H"),1])
shapiro.test(df[which(df$Classe == "L"),1])

shapiro.test(df[which(df$Classe == "H"),2])
shapiro.test(df[which(df$Classe == "L"),2])

# squisitamente normali
var(df[which(df$Classe == "H"),1:2])
var(df[which(df$Classe == "L"),1:2])

plot(df[,1],df[,2],col=df[,3])

attach(df)
library(MASS)
cl <- lda(Classe ~ Lunghezza + Larghezza)


cl <- lda(Classe ~ Lunghezza + Larghezza, CV = T)

# should do cross validation

l <- table(df$Classe, cl$class)

errorCV <- (cl$class != df[,3])
errorCV

AERCV1   <- sum(errorCV)/length(df[,3])
AERCV1

# lunghezza

cl1 <- lda(Classe ~ Lunghezza , CV = T)

# should do cross validation

l <- table(df$Classe, cl1$class)

errorCV <- (cl1$class != df[,3])
errorCV

AERCV2  <- sum(errorCV)/length(df[,3])
AERCV2

# larghezza

cl1 <- lda(Classe ~ Larghezza , CV = T)

# should do cross validation

l <- table(df$Classe, cl1$class)

errorCV3 <- (cl1$class != df[,3])
errorCV3

AERCV3  <- sum(errorCV3)/length(df[,3])
AERCV3

# c

# Assumendo che i costi delle misurazioni di lunghezza e larghezza siano entrambi pari a 10e per bagaglio ed il costo
# di un errato posizionamento di un bagaglio in stiva sia pari a 10e per bagaglio per ogni 100 km di volo:
#   c) scegliete quale classi¯catore µe economicamente piµu vantaggioso utilizzarsi in un volo di 1 000 km con 200
# bagagli in stiva.

cost1 <- (10+10)*200 + AERCV1*10*10*200
cost2 <- 10*200 + AERCV2*10*10*200
cost3 <- 10*200 + AERCV3*10*10*200


