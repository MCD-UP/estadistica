#Prueba de hipótesis
binom.test( x = 2, # los 2 días con niveles ozono superiores
            n = 132, # el total de días, los 132
            p = 0.05, 
            alternative = "less", # en relación a la H.alternativa
            conf.level = 0.95)

# Ejercicio 5
norm <- c( 3.2005, 0.2608, 1.5324, 1.92, 1.4173, 0.0164, -0.9709, 1.8213 )
med <- mean(norm)
sd <- sd(norm)
c( med, sd )

tstat <- (med - 0) / (sd/sqrt(8)) # estadístico t 
#la distribución t es una distribución de probabilidad que surge del problema 
#de estimar la media de una población normalmente distribuida cuando el tamaño de la muestra es pequeño.
gl <- length(norm) - 1 # grados de libertad
tstat
gl

pval <- 2 * pt(-abs(tstat), gl) # p-valor, función de distribución de t
pval

# Ejercicio 6
dfmedi <- read.table(file = "./ds1.csv",
                             header = TRUE,
                             sep = ";",
                             dec = ".",
                             encoding = "UTF-8",
                             stringsAsFactors = FALSE) # carga los datos


dfmedi$genero <- as.factor(dfmedi$genero) # Transformación de los datos a factor

dfmedi$raza <- as.factor(dfmedi$raza) # Transformación de los datos a factor

#SUPUESTO DE NORMALIDAD: Con el gráfico Q-Q se hace una primera aproximación visual de si hay o no normalidad (gráfico descriptivo)
qqnorm(dfmedi$m0) # la nube de puntos 
qqline(dfmedi$m0) # la recta

#contraste de normalidad con el test de Shapiro

shapiro.test ( dfmedi$m0 )
#Interpretación: Con un p-value 0.1433, mayor de 0.05 no se puede rechazar la hipótesis nula (hipótesis de normalidad). 
#Por lo tanto, se concluye que los datos cumplen el supuesto de normalidad.

t.test( dfmedi$m0,
        mu = 43, 
        alternative = "two.sided" ) # contraste bilateral
#Con un p-value = 0.7465, mayor de 0.05 no se puede rechazar la hipótesis nula H0. 
# Se concluye que la media de los valores en el mes inicial m0 es 43. #
#El intervalo de confianza incluye el 43 (38.78284 48.77271).

#inciso b
HombresIni <- dfmedi$m0[dfmedi$genero == "1"]

MujeresIni <- dfmedi$m0[dfmedi$genero == "2"]

qqnorm(HombresIni) # la nube de puntos
qqline(HombresIni) # la recta

shapiro.test ( HombresIni ) # contraste de normalidad
#Con un p-value = 0.06043, mayor de 0.05, no podemos rechazar la hipótesis nula. 
#Por lo tanto, se concluye que nuestros datos cumplen el supuesto de normalidad.

qqnorm( MujeresIni ) # la nube de puntos 
qqline( MujeresIni ) # la recta

shapiro.test ( MujeresIni ) 
#La conclusión es similar, pues 0.178> 0.05. Los datos cumplen con es supuesto de normalidad.

var.test( HombresIni, MujeresIni ) # contraste de homogeneidad de varianzas
# Interpretación: Con un p-value = 0.8238, mayor de 0.05, no podemos rechazar la hipótesis nula. 
#Por lo tanto suponemos homogeneidad de varianzas.

t.test( HombresIni, MujeresIni, # dos muestras 
        alternative = "two.sided", # contraste bilateral 
        paired = FALSE, # muestras independientes
        var.equal = TRUE ) # se supone homocedasticidad

#Con un p-value = 0.2524, mayor de 0.05 no podemos rechazar la hipótesis nula H0 de igualdad de medias. 
#Esto es, no hay diferencias significativas entre las medias. #
#Podemos concluir que la media de los hombres y la media de las mujeres no son distintas para el mes inicial.

#inciso c
qqnorm( dfmedi$m3 - dfmedi$m0 ) # la nube de puntos
qqline( dfmedi$m3 - dfmedi$m0 ) # la recta
shapiro.test ( dfmedi$m3 - dfmedi$m0 )
#Los puntos se agrupan en torno a la recta. 
#En un principio, visualmente se aprecia que los datos cumplen el supuesto de normalidad.

#Con un p-value = 0.6279, mayor de 0.05, no podemos rechazar la hipótesis nula.
#Por lo tanto, podemos concluir que los datos cumplen el supuesto de normalidad.

#CONTRASTE DE HIPÓTESIS: Se supone normalidad en nuestros datos, podemos realizar el contraste.

t.test( dfmedi$m3,
        dfmedi$m0,
        alternative = "two.sided",
        paired = TRUE ) # contraste muestras dependientes

#Con un p-value = 1.614e-10 menor de 0.05 podemos rechazar la hipótesis nula H0 de igualdad de medias. 
#Podemos concluir que existen diferencias entre la media de los valores en el mes inicial m0 
#y la de los valores en el tercer mes m3. Por lo tanto, los investigadores tienen razón.