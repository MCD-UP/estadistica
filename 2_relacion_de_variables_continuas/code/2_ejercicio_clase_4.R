##### Ejercicio 4 #####

# Sarahí Aguilar González

# El archivo notasTest.csv contiene calificaciones de dos exÃ¡menes (inicial y final) aplicados en dos clases diferentes.

data <- read.table(file = "./notasTest.csv",
                   header = TRUE,
                   sep = ";",
                   dec = ".",
                   encoding = "UTF-8",
                   stringsAsFactors = FALSE)

# Se quiere saber si:

# a. La media de las notas del examen inicial es 22.
mean(data$notaInicial) # = 23.45833
# No, la media de las notas del examen inicial es 23.45833.

# b. La media de la clase A es distinta de la de la clase B en el examen final.
mean(data[data$clase == "A", "notaInicial"]) # = 24.125
mean(data[data$clase == "B", "notaInicial"]) # = 22.79167
# Sí, son distintas. En el examen inicial, la media de la clase A es de 24.125; mientras que la de la clase B es 22.79167.

# c. La media de la clase A se modifica en el examen final respecto al inicial.
mean(data[data$clase == "A", "notaInicial"]) # = 24.125
mean(data[data$clase == "A", "notaFinal"]) # = 24.25
# Sí, la clase A obtuvo una calificaciÃ³n media mÃ¡s alta en el examen final que en el inicial.

# d. La media de la clase B se modifica en el examen final respecto al inicial.
mean(data[data$clase == "B", "notaInicial"]) # = 22.79167
mean(data[data$clase == "B", "notaFinal"]) # = 25.45833
# Sí, la clase B obtuvo una calificaciÃ³n media mÃ¡s alta en el examen final que en el inicial.

# e. La media mejora en el examen final respecto del inicial.
mean(data$notaInicial) # = 23.45833
mean(data$notaFinal) # = 24.85417
# Sí, la media mejora en el examen final respecto del inicial.
