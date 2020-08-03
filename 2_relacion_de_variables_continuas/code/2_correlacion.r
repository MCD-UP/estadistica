library(ggplot2)
library(MASS)

dfmedi <- read.table(file = "./ds1.csv",
                     header = TRUE,
                     sep = ";",
                     dec = ".",
                     encoding = "UTF-8",
                     stringsAsFactors = FALSE) 

head(dfmedi) # comprobamos si se leyeron bien

str(dfmedi)

ggplot(data = dfmedi, aes((x = m0), y = m1)) + geom_point()

##### Correlaci�n entre mediciones 0 y 1 #####

# Coeficiente de correlaci�n
r <- cor(dfmedi$m0, dfmedi$m1)

# Coeficiente muestral
r_cuad <- r*r

# 86% de la variaci�n de los valores de Y es ocasionada por una relaci�n lineal con X.
cor(dfmedi$m0, dfmedi$m1, method = "spearman")



##### Correlaci�n entre mediciones 0 y 3 #####

# Obtenemos un valor de correlaci�n positivo y alta, 
# no var�a mucho de la anterior, esto se debe a s� se cumplen las condiciones de la correlaci�n de Pearson.
ggplot(data = dfmedi, aes((x = m0), y = m3)) + geom_point()
r <- cor(dfmedi$m0, dfmedi$m3)
rcuad <- r*r

# 39% de la variaci�n de los valores de Y es ocasionada por una relaci�n lineal con X.
cor(dfmedi$m0, dfmedi$m3, method="spearman")



##### Correlaci�n entre mediciones 1 y 3 #####

ggplot(data=dfmedi,aes((x=m1),y=m3))+geom_point()
cor(dfmedi$m1,dfmedi$m3)
cor(dfmedi$m1,dfmedi$m3,method="spearman")

#Todas las correlaciones en el dataframe
cor(dfmedi,use="pairwise.complete.obs")
# pairwise.complete.obs calcula el coeficiente de correlaci�n para aquellas observaciones en las que no falta
# ning�n valor de "x" ni "y".
