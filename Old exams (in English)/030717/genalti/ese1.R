library(car)
df <- read.table("kimono.txt")
head(df)

a <- anova(lm(value ~ type + city + type:city, data = df))

# Analysis of Variance Table

# Response: value
#            Df  Sum Sq Mean Sq    F value Pr(>F)    
# type        1 29342.6 29342.6 14528.1695 <2e-16 ***
# city        1     1.9     1.9     0.9305 0.3352    
# type:city   1     0.3     0.3     0.1240 0.7249    
# Residuals 524  1058.3     2.0                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# l'interazione non sembra influire, la togliamo

anova(lm(value ~ type + city, data = df))

# Analysis of Variance Table
# 
# Response: value
#            Df  Sum Sq Mean Sq   F value Pr(>F)    
# type        1 29342.6 29342.6 14552.451 <2e-16 ***
# city        1     1.9     1.9     0.932 0.3348    
# Residuals 525  1058.6     2.0                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# verifichiamo le ipotesi per i modelli (in realtà ci interessa che valgano per quello finale)

tokyo <- df[which(df$city == 'Tokyo'),1]
kyoto <- df[which(df$city == 'Kyoto'),1]
hm <- df[which(df$type == 'hand-made'),1]
rtu <- df[which(df$type == 'ready-to-use'),1]

shapiro.test(tokyo)
shapiro.test(kyoto) # ipotesi non verificate

shapiro.test(hm) # ok
shapiro.test(rtu) # ce lo facciamo andare bene
qqnorm(rtu)
qqline(rtu)

bartlett.test(df$value , df$type)
var.test(hm,rtu) # l'ipotesi sulle varianze è accettabile


# solo la variabile type pare influire sul value, il nostro modello ridotto comprenderà solo quella

### Interval at 95% for the differences (reduced additive model)
### [b=2, thus one interval only]
n1 <- dim(df[which(df$type == 'hand-made'),])[1]
n2 <- dim(df[which(df$type == 'ready-to-use'),])[1]
n <- n1 + n2

M1 <- mean(df[which(df$type == 'hand-made'),1])
M2 <- mean(df[which(df$type == 'ready-to-use'),1])
S1 <- var(df[which(df$type == 'hand-made'),1])
S2 <- var(df[which(df$type == 'ready-to-use'),1])
Sp <- sqrt(((n1-1)*S1+(n2-1)*S2)/(n1+n2-2))
delta <- M1 - M2

IC <- c(delta - qt(0.975, n1+n2-2) * Sp * sqrt(1/n1+1/n2), delta + qt(0.975, n1+n2-2) * Sp * sqrt(1/n1+1/n2))
        
names(IC) <- c('Inf', 'Sup')
IC    # IC for mu(ready-to-use)-mu(hand-made)


