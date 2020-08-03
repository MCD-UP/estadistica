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

##### Correlación entre mediciones 0 y 1 #####

# Coeficiente de correlación
r <- cor(dfmedi$m0, dfmedi$m1)

# Coeficiente muestral
r_cuad <- r*r

# 86% de la variación de los valores de Y es ocasionada por una relación lineal con X.
cor(dfmedi$m0, dfmedi$m1, method = "spearman")



##### Correlación entre mediciones 0 y 3 #####

# Obtenemos un valor de correlación positivo y alta, 
# no varía mucho de la anterior, esto se debe a sí se cumplen las condiciones de la correlación de Pearson.
ggplot(data = dfmedi, aes((x = m0), y = m3)) + geom_point()
r <- cor(dfmedi$m0, dfmedi$m3)
rcuad <- r*r

# 39% de la variación de los valores de Y es ocasionada por una relación lineal con X.
cor(dfmedi$m0, dfmedi$m3, method="spearman")



##### Correlación entre mediciones 1 y 3 #####

ggplot(data=dfmedi,aes((x=m1),y=m3))+geom_point()
cor(dfmedi$m1,dfmedi$m3)
cor(dfmedi$m1,dfmedi$m3,method="spearman")

#Todas las correlaciones en el dataframe
cor(dfmedi,use="pairwise.complete.obs")
# pairwise.complete.obs calcula el coeficiente de correlación para aquellas observaciones en las que no falta
# ningún valor de "x" ni "y".
