# ---
# title: "Dependencias"
# author: "Daniel Villegas"
# date: "5/12/2020"
# output: pdf_document
# ---
  
require(gdata)
df<- file.path(path.package('gdata'),'xls',
               'BD__polimorf.xlsx')


## R Markdown

# Preparamos los datos, haciendo una conversión de los datos de factores a numéricos para poder obtener la frecuencia.


pol1<-as.numeric(as.factor(df$POL1))
pol2<-as.numeric(as.factor(df$POL2))
pol3<-as.numeric(as.factor(df$POL3))
hta<-as.numeric(as.factor(df$HTA))
tabq<-as.numeric(as.factor(df$TABQ))
alcohol<-as.numeric(as.factor(df$ALCOHOL))

indepHTA<-data.frame(pol1,pol2,pol3,hta)
indepTABQ<-data.frame(pol1,pol2,pol3,tabq)
indepALCH<-data.frame(pol1,pol2,pol3,alcohol)

##HTA

# Hacemos una prueba de independencia para la HTA:
  

x<-table(indepHTA$pol1,indepHTA$hta)
addmargins(x) 
odf1 <- t(data.frame(c(60,44,22),c(61,52,17),c(121,96,39)))
chisq.test(odf1)


# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 1.2538, por lo que rechazamos la hipótesis nula.


x<-table(indepHTA$pol2,indepHTA$hta)
addmargins(x) 
odf1 <- t(data.frame(c(14,105,7),c(11,118,1),c(25,223,8)))

chisq.test(odf1)


# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 5.5567, por lo que rechazamos la hipótesis nula.


x<-table(indepHTA$pol3,indepHTA$hta)
addmargins(x) 
odf1 <- t(data.frame(c(5,84,37),c(12,76,42),c(17,160,79)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 3.5372, por lo que rechazamos la hipótesis nula.


##TABAQUISMO


x<-table(indepTABQ$pol1,indepTABQ$tabq)
addmargins(x) 
odf1 <- t(data.frame(c(96,76,32),c(25,20,7),c(32,7,39)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 47.341, por lo que aceptamos la hipótesis nula.


x<-table(indepTABQ$pol2,indepTABQ$tabq)
addmargins(x) 
odf1 <- t(data.frame(c(24,175,5),c(1,48,3),c(25,223,8)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 5.7723, por lo que rechazamos la hipótesis nula.


x<-table(indepTABQ$pol3,indepTABQ$tabq)
addmargins(x) 
odf1 <- t(data.frame(c(16,119,69),c(1,41,10),c(17,160,79)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 7.8361, por lo que aceptamos la hipótesis nula.

##ALCOHOL


x<-table(indepALCH$pol1,indepALCH$alcohol)
addmargins(x) 
odf1 <- t(data.frame(c(110,87,36),c(11,9,3),c(121,96,39)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 0.099215, por lo que rechazamos la hipótesis nula.


x<-table(indepALCH$pol2,indepALCH$alcohol)
addmargins(x) 
odf1 <- t(data.frame(c(23,204,6),c(2,19,2),c(25,223,8)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 2.5978, por lo que rechazamos la hipótesis nula.



x<-table(indepALCH$pol3,indepALCH$alcohol)
addmargins(x) 
odf1 <- t(data.frame(c(17,140,76),c(0,20,3),c(17,160,79)))

chisq.test(odf1)

# Con dos grados de libertad tenemos una X=5.99 y con Chi²  obtenemos 6.6957, por lo que aceptamos la hipótesis nula.
