##### Ejercicio 2 #####

# Sarah� Aguilar Gonz�lez

# Se ha lanzado un dado 275 veces, de las que 60 ha salido el 6.
# Si el dado no está cargado, esperamos que el 6 salga 275/6 = 45.8333333 veces.
# ¿Es razonable pensar con 95% de confianza que el dado no está trucado?

binom.test(60, 275, p = 1/6)

# Exact binomial test
#
# data:  60 and 275
# number of successes = 60, number of trials = 275, p-value = 0.02847
# alternative hypothesis: true probability of success is not equal to 0.1666667
# 95 percent confidence interval: 0.1708227 0.2717267
# sample estimates: probability of success 0.2181818

# No es razonable pensar que el dado no está truncado, tomando en cuenta que el p-value es de 0.02847.
