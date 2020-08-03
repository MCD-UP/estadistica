install.packages("modeest")
install.packages("FinCal")
library(modeest)
library(FinCal)

x = rep(c(2, 7, 15, 30), c(47, 32, 17, 4))
length(x)
mean(x)
var(x)
sd(x)

# Moda, library(modeest)
mfv(x)

# Coeficiente de variacion, library(FinCal)
coefficient.variation(sd = sd(x), avg = mean(x))
# Muestra muy heterogenea


# Variables tipificadas
z <- scale(x, center= T)
# La tipificación es el proceso de transformar, trasladar o centrar
# los valores que toma una variable X a valores
# que están estandarizados, aplicando un cambio de variable.

z
length(z)
mean(z)
var(z)
sd(z)

media, moda, varianza, desviacion, max, min, quartiles,
