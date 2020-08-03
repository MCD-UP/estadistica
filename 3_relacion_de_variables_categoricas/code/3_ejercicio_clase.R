library(haven)
library(dplyr) 

CEP <-read_spss("c:/Users/anton/Documents/Clases UP/ESTADISTICA_MCD/CEP2.sav")
CEP2 <- select(CEP, edad = DS_P2_EXACTA,satisfaccion_vida = SV_1, satisfaccion_ciudadana = SV_2)
#Se indica base de datos, el nombre de variable a crear y los datos que la compondrán.
View(CEP2) #Visualización de la base

min <- min(CEP2$satisfaccion_vida, na.rm = TRUE)
q1 <- quantile(CEP2$satisfaccion_vida, probs = 0.25, na.rm = TRUE)
media <- mean.default(CEP2$satisfaccion_vida, na.rm = TRUE)
mediana <- median.default(CEP2$satisfaccion_vida, na.rm = TRUE)
moda <- mfv(CEP2$satisfaccion_vida)
var <- var(CEP2$satisfaccion_vida, na.rm = TRUE)
desvest <- sd(CEP2$satisfaccion_vida, na.rm = TRUE)
q3 <- quantile(CEP2$satisfaccion_vida, probs = 0.75, na.rm = TRUE)
max <- max(CEP2$satisfaccion_vida, na.rm = TRUE)


range(CEP2$satisfaccion_vida)
range(CEP2$satisfaccion_ciudadana)
range(CEP2$edad)

library(psych)
skew(CEP2$satisfaccion_vida)
#coeficiente de asimetría mayor que cero, asimetría a la derecha
kurtosi(CEP2$satisfaccion_vida)
#El coeficiente de curtosis puede usarse como un indicador, 
#en combinación de otros, de la posible existencia de observaciones anómalas, de no normalidad.
#Mayor que cero,la distribución es más apuntada y con colas más gruesas que la normal.


skew(CEP2$satisfaccion_ciudadana)
#coeficiente de asimetría mayor que cero, asimetría a la derecha
kurtosi(CEP2$satisfaccion_ciudadana)
#la distribución es más apuntada y con colas más gruesas que la normal.

skew(CEP2$edad)
#coeficiente de asimetría menor que cero, asimetría a la derecha
kurtosi(CEP2$edad)
#coeficiente de asimetríamenor que cero
#la distribución es menos apuntada y con colas menos gruesas que la normal.

shapiro.test(CEP2$edad)# se rechaza la normalidad
shapiro.test(CEP2$satisfaccion_vida)
shapiro.test(CEP2$satisfaccion_ciudadana)

x<-quantile(CEP2$satisfaccion_vida)
hist(x,main = "Histograma", xlab="valores de x",ylab = "Frecuencia",col = "purple",breaks =4)
y<-quantile(CEP2$satisfaccion_ciudadana)
hist(y,main = "Histograma", xlab="valores de x",ylab = "Frecuencia",col = "purple",breaks =4)
z<-quantile(CEP2$edad)
hist(z,main = "Histograma", xlab="valores de x",ylab = "Frecuencia",col = "purple",breaks =4)

summary(CEP2$satisfaccion_vida)
summary(CEP2$satisfaccion_ciudadana)
summary(CEP2$edad)
