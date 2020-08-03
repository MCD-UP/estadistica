x = c(2.2, 7.6, 2.9, 4.6, 4.1, 3.9, 7.4, 3.2, 5.1, 5.3, 20.1, 2.3, 5.5, 32.7, 9.1, 1.7, 3.2, 5.8,16.3, 15.9, 5.9, 6.7, 3.4, 40.5)
length(x)
mean(x)
median(x)
sd(x)

fivenum(x)
# el resumen de 5 n?meros contiene la punt.menor, el cuarto inferior (aprox P25),
#la mediana,el cuarto superior (aprox P75) y la punt.mayor summary(x)

p25=quantile(x,.25)
p25
p50=quantile(x,.50)
p50
p75=quantile(x,.75)
p75
quantile(x)
IQR(x)
barplot(quantile(x))
#percentiles 25, 50 y 75
#El percentil es una medida de posici?nque indica, una vez ordenados
#los datos de menor a mayor, el valor de la variable por debajo del que
#se encuentra un porcentaje dado de observaciones en un grupo

asimetria=function(x) {
  m3=mean((x-mean(x))^3)
  a=m3/(sd(x)^3)
  a}
asimetria(x)

kurtosis=function(x) {
  m4=mean((x-mean(x))^4)
  kurt=m4/(sd(x)^4)-3
  kurt}
kurtosis(x)

hist(x,main = "Histograma", xlab="valores de x", ylab = "Frecuencia", col = "purple", breaks = 4)
boxplot(x, main = "Datos at?picos", ylab = "valores de x", col = "purple")


y = log(x) 1/x x*x
hist(y, main = "Histograma", xlab="valores de ln(x)", ylab = "Frecuencia", col = "green", breaks = 4)
boxplot(y, main = "Datos at?picos", ylab = "valores de ln(x)", col = "green")
