##### Ejercicio 3 #####

# Sarah� Aguilar Gonz�lez

# Se estima que la proporción de adultos que vive en una pequeña ciudad que son graduados universitarios es p = 0.6.
# Para probar esta hipótesis se selecciona una muestra aleatoria de 15 adultos.
# Si el número de graduados en la muestra es cualquier número entre 6 y 12, no rechazaremos la hipótesis nula de que p = 0.6; de otro modo, concluiremos que p ≠ 0.6.

# a) Evalúe 𝛼  suponiendo que p = 0.6. Utilice la distribución binomial.
pbinom(5, 15, 0.6, lower.tail = TRUE) + (1 - pbinom(12, 15, 0.6, lower.tail = TRUE))

# b) Evalúe β para las alternativas p = 0.5 y p = 0.7.

# p = 0.5
pbinom(12, 15, 0.5, lower.tail = TRUE) - pbinom(5, 15, 0.5, lower.tail = TRUE)

# p = 0.7
pbinom(12, 15, 0.7, lower.tail = TRUE) - pbinom(5, 15, 0.7, lower.tail = TRUE)

# c)¿Es éste un buen procedimiento de prueba?
# La probabilidad de cometer un error de tipo 1 es de 0.05, valor que no resulta lo suficientemente bueno.
# Además, las probabilidades de comr un error de tipo 2 son extremadamente altas.
# Por lo tanto, este no es un buen procedimiento de prueba.
