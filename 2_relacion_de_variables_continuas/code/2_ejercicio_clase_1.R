##### Ejercicio 1 #####

# Sarah� Aguilar Gonz�lez

# El conjunto de datos cancer.csv contiene los resultados de un estudio que mide las capacidades orales de enfermos de cáncer de garganta.
# Las medidas están tomadas inicialmente y a las 2, 4 y 6 semanas de tratamiento.
# Además las variables edad, peso inicial y estado inicial del cáncer fueron medidas para cada paciente.
# En el hospital, a un grupo se le administra un placebo (0) y al otro un tratamiento (1).

data <- read.table(file = "./cancer.csv",
                   header = TRUE,
                   sep = ";",
                   dec = ".",
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE)

# a. Determine si hay diferencias en la media de la capacidad oral de los enfermos en la segunda semana (Variable TOTALCW2)
# según el grupo de edad (AGE) al que pertenece el enfermo.

age_1 <- data$TOTALCW2[data$AGE == "1"]
age_2 <- data$TOTALCW2[data$AGE == "2"]

# Contraste de normalidad para grupo de edad 1
qqnorm(age_1) # La nube de puntos
qqline(age_1) # La recta

shapiro.test(age_1)
# Con un p-value = 0.0552, mayor de 0.05, no podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que nuestros datos cumplen el supuesto de normalidad.

# Contraste de normalidad para grupo de edad 2
qqnorm(age_2) # La nube de puntos
qqline(age_2) # La recta

shapiro.test(age_2)
# Con un p-value = 0.01638, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que nuestros datos no cumplen el supuesto de normalidad.

# Contraste de homogeneidad de varianzas
var.test(age_1, age_2)
# Con un p-value = 0.9874, mayor de 0.05, no podemos rechazar la hipótesis nula H0 de igualdad de varianzas.
# Esto es, no hay diferencias significativas entre las varianzas.
# Podemos concluir que la varianza de los de edad 1 y la varianza de los de edad 2 no son distintas para el mes inicial.

# Contraste de homogeneidad de medias
t.test(age_1, age_2, # Muestras por edad 1 y 2
       alternative = "two.sided", # Contraste bilateral
       paired = FALSE, # Muestras independientes
       var.equal = TRUE ) # Se supone homocedasticidad

# Con un p-value = 0.1199, mayor de 0.05, no podemos rechazar la hipótesis nula H0 de igualdad de medias.
# Esto es, no hay diferencias significativas entre las medias.
# Podemos concluir que la media de los de edad 1 y la media de los de edad 2 no son distintas para el mes inicial.

# b. Determine si hay diferencias en la media de la capacidad oral de los enfermos en la segunda semana (Variable TOTALCW2)
# según el tratamiento que se le da (TRT).

trt_0 <- data$TOTALCW2[data$TRT == "0"]
trt_1 <- data$TOTALCW2[data$TRT == "1"]

# Contraste de normalidad para grupo de edad 1
qqnorm(trt_0) # La nube de puntos
qqline(trt_0) # La recta

shapiro.test(trt_0)
# Con un p-value = 0.01063, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que nuestros datos no cumplen el supuesto de normalidad..

# Contraste de normalidad para grupo de edad 2
qqnorm(trt_1) # La nube de puntos
qqline(trt_1) # La recta

shapiro.test(trt_1)
# Con un p-value = 0.4982, mayor a 0.05, no podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que nuestros datos cumplen el supuesto de normalidad.

# Contraste de homogeneidad de varianzas
var.test(trt_0, trt_1)
# Con un p-value = 0.8409, mayor de 0.05, no podemos rechazar la hipótesis nula H0 de igualdad de varianzas.
# Esto es, no hay diferencias significativas entre las varianzas.
# Podemos concluir que la varianza de los de TRT 0 y la varianza de los de TRT 1 no son distintas para el mes inicial.

# Contraste de homogeneidad de medias
t.test(trt_0, trt_1, # Muestras por edad 1 y 2
       alternative = "two.sided", # Contraste bilateral
       paired = FALSE, # Muestras independientes
       var.equal = TRUE ) # Se supone homocedasticidad

# Con un p-value = 0.7502, mayor de 0.05, no podemos rechazar la hipótesis nula H0 de igualdad de medias.
# Esto es, no hay diferencias significativas entre las medias.
# Podemos concluir que la media de los de TRT 0 y la varianza de los de TRT 1 no son distintas para el mes inicial.
