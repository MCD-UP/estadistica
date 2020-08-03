dat <- haven::read_sav("./CEP2.sav")

# Función que imprime descriptivos dada una columna de un dataframe
PrintDescriptivos <- function(x){
  print(paste0("Mínimo: ", toString(min(x))))
  print(paste0("Máximo: ", toString(max(x))))
  print(paste0("Media: ", toString(mean(x))))
  print(paste0("Mediana: ", toString(median(x))))
  print(paste0("Moda: ", toString(modeest::mfv(x))))
  print(paste0("Varianza: ", toString(var(x))))
  print(paste0("Desviación estándar: ", toString(sd(x))))
  print(paste0("Quartiles: ", toString(quantile(x))))
}

PrintDescriptivos(dat$SV_1)
PrintDescriptivos(dat$SV_2)
PrintDescriptivos(dat$DS_P2_EXACTA)

# Determinar si las variables tienen comportamiento normal

shapiro.test(dat$SV_1)
# Con un p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que la variable SV_1 no cumple el supuesto de normalidad.

shapiro.test(dat$SV_2)
# Con un p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que la variable SV_2 no cumple el supuesto de normalidad.

asimetria <- function(x) {
  m3 = mean((x - mean(x))^3)
  a = m3/(sd(x)^3)
  a
}
asimetria(dat$SV_1)
asimetria(dat$SV_2)
asimetria(dat$DS_P2_EXACTA)

kurtosis <- function(x) {
  m4 = mean((x-mean(x))^4)
  kurt = m4/(sd(x)^4)-3
  kurt
}
kurtosis(dat$SV_1)
kurtosis(dat$SV_2)
kurtosis(dat$DS_P2_EXACTA)

##### Tranformaciones #####

# Histograma original de SV_1
sv1 <- dat$SV_1

hist(sv1, main = "Histograma", xlab = "Valores de x", ylab = "Frecuencia", col = "purple", breaks = 4)

# Transformación 1 de SV_1
sv1_t1 = log(sv1)
hist(sv1_t1, main = "Histograma", xlab = "Valores de x", ylab = "Frecuencia", col = "purple", breaks = 4)

shapiro.test(sv1_t1)
asimetria(sv1_t1)
kurtosis(sv1_t1)

# Con un p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que la variable SV_1 transformada no cumple el supuesto de normalidad.

# Histograma original de SV_1
sv2 <- dat$SV_2

hist(sv2, main = "Histograma", xlab = "Valores de x", ylab = "Frecuencia", col = "purple", breaks = 4)

# Transformación 2 de SV_2
sv2_t1 = log(sv2)
hist(sv2_t1, main = "Histograma", xlab = "Valores de x", ylab = "Frecuencia", col = "purple", breaks = 4)

shapiro.test(sv2_t1)
asimetria(sv2_t1)
kurtosis(sv2_t1)

# Con un p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipótesis nula.
# Por lo tanto, se concluye que la variable SV_2 transformada no cumple el supuesto de normalidad.


