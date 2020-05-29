df <- read.table("kimono.txt")
head(df)

anova(lm(value ~ type + city + type:city, data = df))


# Analysis of Variance Table

# Response: value
# Df  Sum Sq Mean Sq    F value Pr(>F)    
# type        1 29342.6 29342.6 14528.1695 <2e-16 ***
#   city        1     1.9     1.9     0.9305 0.3352    
# type:city   1     0.3     0.3     0.1240 0.7249    
# Residuals 524  1058.3     2.0                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# l'interazione non sembra influire, la togliamo

anova(lm(value ~ type + city, data = df))

# Analysis of Variance Table
# 
# Response: value
# Df  Sum Sq Mean Sq   F value Pr(>F)    
# type        1 29342.6 29342.6 14552.451 <2e-16 ***
#   city        1     1.9     1.9     0.932 0.3348    
# Residuals 525  1058.6     2.0                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# solo la variabile type pare influire sul value, il nostro modello ridotto comprenderà solo quella

### Note: These aren't the only tests we can do!
### Example: global test for the significance of the two treatments 
###          (model without interaction)

kvalue          <- df$value
kcity      <- factor(df$city)
ktype        <- factor(df$type)

g <- length(levels(kcity))
b <- length(levels(ktype))
n_type <- length(df)

M           <- mean(kvalue)
Mcity      <- tapply(kvalue, kcity, mean)
Mtype      <- tapply(kvalue, ktype, mean)

SStype <- sum(n*b*(Mtype - M)^2)                
SScity  <- sum(n*g*(Mcity  - M)^2)            
SSres   <- sum((kvalue - M)^2) - (SStype+SScity)   

Ftot      <- ( (SStype + SScity) / ((g-1)+(b-1)))/(SSres / (n*g*b-g-b+1))
Ptot      <- pf(Ftot, (g-1)+(b-1), n*g*b-g-b+1) # attention to the dgf!
Ptot
# non ho evidenza per dire che anche city pesi nel modello, ulteriore riprova

### Reduced additive model (ANOVA one-way, b=2): 
### X.jk = mu + beta.j + eps.jk; eps.jk~N(0,sigma^2), 
###     j=1,2 (effect type)
fit.aov1 <- aov(kvalue ~ ktype)
summary.aov(fit.aov1)

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

SSres <- sum(residuals(fit.aov1)^2)

### Interval at 95% for the differences (reduced additive model)
### [b=2, thus one interval only]
IC <- c(diff(Mtype) - qt(0.975, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))), 
        diff(Mtype) + qt(0.975, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
names(IC) <- c('Inf', 'Sup')
IC    # IC for mu(ready-to-use)-mu(hand-made)




