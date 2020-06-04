kimono <- read.csv("C:/Users/ffede/OneDrive/Desktop/AppliedStatTDE/Old exams (in English)/030717/kimono.txt", sep="")

attach(kimono)

### normality check


shapiro.test(value[ city==levels(city)[1] ])
shapiro.test(value[ city==levels(city)[2] ])

shapiro.test(value[ type==levels(type)[1] ])
shapiro.test(value[ type==levels(type)[2] ])
#normality not respected for type

### equal variance check

var(value[ city==city[1] ])
var(value[ city==city[2] ])

var(value[ type==levels(type)[1] ])
var(value[ type==levels(type)[2] ])

bartlett.test(value, city)
bartlett.test(value, type)
# variance is the same 

aov_all <- aov(lm(value ~ city + type + city:type))
summary(aov_all)
# type e interazione non sono significativi (type[1] in realtà non
# è pienamente normale però...)

### Modello ridotto
aov_r <- aov(lm(value ~ type))
summary(aov_r)
#boxplot(value ~ type, data = kimono)

mean_type <- tapply(kimono$value, kimono$type, mean)

n       <- length(type)      # total number of obs.
ng      <- table(type)       # number of obs. in each group    
g       <- length(levels(type)) 

k <- g*(g-1)/2
alpha= 0.05
SSres <- sum(residuals(aov_r)^2)
S <- SSres/(n-g)

# Quale di questi intervalli è corretto?
paste(levels(type)[1],"-",levels(type)[2])
as.numeric(c(mean_type[1]-mean_type[2] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2])),
             mean_type[1]-mean_type[2] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2]))))


detach(kimono)