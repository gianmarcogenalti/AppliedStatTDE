library(car)
#install.packages('interactions')
library(interactions)
rm(list = ls())
df <- read.table('chinatown.txt')
head(df)
N <- dim(df)[1]

kl <- rep(NA,N)

for(i in 1:N){
  if(df[i,3] == 'M' & df[i,4] == 'Europa'){
    kl[i] <- 'me'
  }
  if(df[i,3] == 'F' & df[i,4] == 'Europa'){
    kl[i] <- 'fe'
  }
  if(df[i,3] == 'M' & df[i,4] == 'Asia'){
    kl[i] <- 'ma'
  }
  if(df[i,3] == 'F' & df[i,4] == 'Asia'){
    kl[i] <- 'fa'
  }
  
}

df$k <- kl

mod1 <- lm(peso ~ 0 + giorni:k, data = df)
summary(mod1)
coefs1 <- coef(mod1)
interact_plot(mod1, pred = giorni, modx = k, plot.points = TRUE)

# non sembra esserci interazione tra i gruppi, la rimuoviamo

mod2 <- lm(peso ~ giorni:sesso + giorni:gruppo + 0, data = df)
summary(mod2)  
coefs2 <- coef(mod2)
  
linearHypothesis(mod2, c(1,-1,0), 0) # il sesso differenzia
linearHypothesis(mod2, c(0,0,1), 0) # il gruppo no


mod3 <- lm(peso ~ giorni:sesso+ 0, data = df)
summary(mod3)    
coefs3 <- coef(mod3)  
  
  
  
  
  
  


