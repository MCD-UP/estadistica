# Sarahí Aguilar González

library(datasets)
data("chickwts")
summary(chickwts)

# Muestreo aleatorio simple con repetición
indices <- sample(1:nrow(chickwts), 35, replace = TRUE)
chickwts_m1 <- chickwts[indices, ]
summary(chickwts_m1)

# Muestreo aleatorio simple sin reemplazo 
library(sampling)
estratos <- strata(chickwts, stratanames = c("feed"), size = c(5, 5, 5, 5, 5, 5), method = "srswor")
chickwts_m2 <- getdata(chickwts, estratos)
table(chickwts_m2$feed)
summary(chickwts_m2)

# Muestreo aleatorio simple con reemplazo 
library(sampling)
estratos <- strata(chickwts, stratanames = c("feed"), size = c(5, 5, 5, 5, 5, 5), method = "srswr")
chickwts_m3 <- getdata(chickwts, estratos)
table(chickwts_m3$feed)
summary(chickwts_m3)

# Estimador: desviación estándar
sd(chickwts$weight)
sd(chickwts_m1$weight)
sd(chickwts_m2$weight)
sd(chickwts_m3$weight)

# Estimador: P-value for Shapiro-Wilk normality test 
shapiro.test(chickwts$weight)
shapiro.test(chickwts_m1$weight)
shapiro.test(chickwts_m2$weight)
shapiro.test(chickwts_m3$weight)

# Histogramas
hist(chickwts$weight)
hist(chickwts_m1$weight)
hist(chickwts_m2$weight)
hist(chickwts_m3$weight)
