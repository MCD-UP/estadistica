# Ejemplo de creacion de un vector de variables aleatorias con distribuci�n normal, y media 725
x <- rnorm(40, 725) # Creaci�n de una variable aleatoria de media 725
media <- 670

# Se probar� si la media muestral es 670 por medio del estad�stico tde Student 
test <- t.test(x, mu = media) # Comparaci�n de la media muestral con la media
print(test)
# Los trabajadores de la muestra ganan m�s que las mujeres

####################

# Ejercicio 2 producci�n diaria 
x <- rnorm(50, 871)
sd(x)
test1 <- t.test(x, mu=880)
test1

#Se rechaza la hp�tesis nula porque p<0.05
#Entonces se concluye que la media no es igual a 880 
test2<-t.test(x,mu=870)
test2
# ahora se prueba con la media 870
beta<-pnorm(874.18,mean=870,sd=1.05,lower.tail=TRUE) #sd 21
beta
potencia<- (1-beta)
potencia

####################

# Ejercicio 3
x <- rnorm(36, 65) # Creaci�n de una variable aleatoria de media 725

media <- 68

# Se probar� si la media muestral es 68 por medio del estad�stico tde Student 
test <- t.test(x, mu=media) # Comparaci�n de la media muestral con la media
print(test)
beta<-pnorm(69.9, mean=68, sd = sd(x), lower.tail=TRUE) # 3.6
beta


#Ejercicio 4
binom.test(x = 2, # los 2 d�as con niveles ozono superiores 
           n = 132, # el total de d�as, los 132 
           p = 0.05, alternative = "less", # en relaci�n a la H.alternativa (menor o igual)
           conf.level = 0.95)



