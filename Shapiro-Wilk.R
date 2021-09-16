#fonte: https://www.sheffield.ac.uk/polopoly_fs/1.885202!/file/95_Normality_Check.pdf

#checar por normalidade para testes paramétricos

#sem outliers
my_data2

#histograma
hist(my_data2$Total)
lines(density(my_data2$Total), col = 2)

#q-q plot
qqnorm(my_data2$Total)
qqline(my_data2$Total)

#Shapiro-Wilk
shapiro.test(my_data2$Total)

#Shapiro-Wilk normality test
#data:  my_data2$Total
#W = 0.58571, p-value < 2.2e-16
# os dados não são normais quando p < 0.05
