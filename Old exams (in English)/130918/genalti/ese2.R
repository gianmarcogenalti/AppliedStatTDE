df <- read.table("waiting.txt")
head(df)
attach(df)
levels(course)
levels(city)
mod <- lm(waiting ~ course + city + course:city)
anova(mod)
# 
# Analysis of Variance Table
# 
# Response: waiting
# Df Sum Sq Mean Sq  F value Pr(>F)    
# course        2 100022   50011 859.5448 <2e-16 ***
# city          1      1       1   0.0137 0.9068    
# course:city   2     65      33   0.5594 0.5726    
# Residuals   174  10124      58                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

bartlett.test(waiting ~ course, data = df)
bartlett.test(waiting ~ city, data = df)
# ok ho stessa varianza

qqnorm(waiting)
shapiro.test(waiting)
for (i in levels(course)){
  pval <- shapiro.test(waiting[course == i])$p
  print(pval)
}
# 0.753786  0.5482351  0.3779857 normalità rispetto ad ogni course

for (i in levels(city)){
  pval <- shapiro.test(waiting[city == i])$p
  print(pval)
}
# 2.056531e-08  3.663365e-08 non normalità rispetto alle città

# course sembra molto influente, non si può dire lo stesso della città e della loro interazione, peraltro
# i dati non sono nemmeno normali rispetto alle città quindi tolgo quella variabile

detach(df)
