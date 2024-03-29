rm(list = ls())
load('mcshapiro.test.RData')
df<- read.table('trulli.txt')
head(df)

mcshapiro.test(df)# i dati sono normali multivariati pvalue = 0.5996

np = c()
for(i in 1:3){
  np <- c(np,shapiro.test(df[,i])$p)
}
# pvalues = 0.1208939, 0.1191180, 0.3169064 sufficienti per normalit�

meas <- c(5,2,2.5)
tt = c()
for(i in 1:3){
  tt <- c(tt, t.test(df[,i], mu = meas[i]))
}
tt$p.value
# pvalues = 0.066649595, 0.658838515, 0.001465107 con confidenza del 95% posso dire che le prime due misure siano quelle
                                               #  mentre la terza � diversa con confidenza del 99%
alpha = .05
n <- dim(df)[1]
q <- dim(df)[2]
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 
Md <- colMeans(df)
Sd <- var(df)
IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) )
#                      Md         
# diametro 4.567986 4.816200 5.064414
# altezza1 1.844248 1.976638 2.109027
# altezza2 2.540573 2.670263 2.799952

df$C <- df[,1]*pi
Md <- colMeans(df)
Sd <- var(df)

alpha = .05
n <- dim(df)[1]
q <- dim(df)[2]
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 
Md <- colMeans(df)
Sd <- var(df)
IC.T2 <- cbind( Md - sqrt(cfr.fisher*diag(Sd)/n) , Md, Md + sqrt(cfr.fisher*diag(Sd)/n) )

#                        Md          
# diametro  4.530058  4.816200  5.102342
# altezza1  1.824019  1.976638  2.129256
# altezza2  2.520757  2.670263  2.819768
# C        14.231598 15.130539 16.029479





