library(datasets)
data(iris)
summary(iris)

# Sample takes a sample of the specified size from the elements of x using either with or without repetition
indices <- sample(1:nrow(iris), 60)
iris.muestreado <- iris[indices, ]
summary(iris.muestreado)

# Muestreo aleatorio simple con repetición
indices_cr <- sample(1:nrow(iris), 60, replace = TRUE)
iris.muestreado_cr <- iris[indices_cr, ]
summary(iris.muestreado_cr)

# Muestreo aleatorio simple sin reemplazo (se conservan los mínimos y máximos)
library(sampling)
estratos <- strata(iris, stratanames = c("Species"), size = c(20,20,20), method = "srswor")
iris.muestreado <- getdata(iris, estratos)
table(iris.muestreado$Species)
summary(iris.muestreado)

# Muestreo aleatorio simple con reemplazo (generalmente se puede con reemplazo, pero a veces no se puede devolver la muestra)
estratos_cr <- strata( iris, stratanames = c("Species"), size = c(20,20,20), method = "srswr")
iris.muestreado <- getdata( iris, estratos_cr )
table(iris.muestreado$Species)
summary(iris.muestreado)

