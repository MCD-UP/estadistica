
x = rep(0:5, c(14,10,15,26,20,15))

length(x)

mean(x)

median(x)

p25 = quantile(x,.25)
p25

p50 = quantile(x,.50)
p50

p75 = quantile(x,.75)
p75

quantile(x)
IQR(x)
barplot(quantile(x))
