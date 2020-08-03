gas = scan('http://verso.mat.uam.es/~joser.berrendero/datos/gas6677.dat')
plot(gas)

gas.ts = ts(gas, start = c(1966,1), frequency = 12)
print(gas.ts)
plot(gas.ts)

boxplot(gas.ts ~ cycle(gas.ts))

cycle(gas.ts)


# Media móvil exponencial
series <- c(17, 21, 19, 23, 18, 16, 20, 18, 22, 20, 15, 22)

alfa <- 1

series_est <- vector()
for(i in 1:12){
  if(i == 1){
    series_est[i] <- series[i]
  }else{
    series_est[i] <- alfa*series[i-1] + (1-alfa)*series_est[i-1]
  }
}

plot(series)
plot(series_est)  

###

library(vars)

data("Canada")
str(Canada)

plot(Canada[, "U"], main='Desempleo', xlab='Mes/Año', ylab='Tasa')
lines(rollmean(Canada[, "U"], 4), col="red", lwd=2)
lines(rollmean(Canada[, "U"], 4, align="right"), col="blue", lwd=2)
legend("bottomleft", 
       c("Original", 
         "Media móvil centrada",
         "Media móvil no centrada"),
       lwd=c(1,2,2), 
       col=c("black", "red", "blue"))
grid()

# Transformación exponencial
plot(HoltWinters(gas.ts, beta = FALSE, gamma = FALSE))

# Descomposición de una serie de tiempo
gas.ts.desc = decompose(gas.ts)
plot(gas.ts.desc, xlab='Año')

#####
# Elimicación de la varianza
plot(log(gas.ts))
x = log(gas.ts)
plot(x)

# Eliminación de la tendencia
dif1.x = diff(x)
plot(dif1.x)

# Eliminación de la estacionalidad
dif12.dif1 = diff(dif1.x, lag = 12)
plot(dif12.dif1)
#####

