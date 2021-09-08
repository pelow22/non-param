#fonte: http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r


library(dplyr)
library(ggpubr)
#carregar a tabela
#Dados de acidentes ofídicos no Paraná organizados pela Laura.



my_data <- read.csv(file = "data_subset.csv", sep = ";", header = TRUE)

#checar a tabela
head (my_data)
summary(my_data)
class(my_data)
dim(my_data)

#estatísticas descritivas dos acidentes totais por formação vegetal

group_by(my_data, veg) %>%
    summarise(
        count = n(),
        mean = mean(Total, na.rm = TRUE),
        sd = sd(Total, na.rm = TRUE),
        median = median(Total, na.rm = TRUE),
        IQR = IQR(Total, na.rm = TRUE)
    )

# A tibble: 5 x 6
#veg                              count  mean    sd median   IQR
#<chr>                            <int> <dbl> <dbl>  <dbl> <dbl>
#1 Estepe                              10 1926  3019.  1450. 1352.
#2 Floresta Estacional Semidecidual   227  220.  497.    93   179
#3 Floresta Ombrofila Densa             5  813.  661.   644   604
#4 Floresta Ombrofila Mista           145  792. 2751.   318   564
#5 Savana                               9  410.  356.   246   608

#remover outliers (2 linhas)
my_data2 <- my_data[-c(95, 276),]

#Boxplot

ggboxplot(my_data2, x = "veg", y = "Total",
          color = "veg", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
          ylab = "Acidentes ofídicos (total)", xlab = "Formação Vegetal")

#Mean plots

ggline(my_data2, x = "veg", y = "Total",
       add = c("mean_se", "jitter"),
       order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
       ylab = "Acidentes ofídicos (total)", xlab = "Formação Vegetal")

#Queremos saber se há alguma diferença significativa na média total de acidente ofidico entre as cinco
#formações vegetais.

kruskal.test(Total ~ veg, data = my_data2)

#Kruskal-Wallis rank sum test
#data:  Total by veg
#Kruskal-Wallis chi-squared = 87.852, df = 4, p-value < 2.2e-16

#Como o p-value < 0.05, conclui0se que tem diferenças significativas entre as formações
#vegetais.

#comparação par a par entre as formações vegetais.

pairwise.wilcox.test(my_data2$Total, my_data2$veg,
                     p.adjust.method = "none")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction

#data:  my_data2$Total and my_data2$veg

#                                 Estepe  Floresta Estacional Semidecidual Floresta Ombrofila Densa Floresta Ombrofila Mista
#Floresta Estacional Semidecidual 6.2e-05 -                                -                        -
#Floresta Ombrofila Densa         0.898   0.016                            -                        -
#Floresta Ombrofila Mista         0.031   < 2e-16                          0.330                    -
#Savana                           0.050   0.014                            0.364                    0.667

#P value adjustment method: none

#Formações diferentes quanto: EST x FES; FES x FOD; FES x FOM; FES x SAV


############################Bothrops
my_data2

group_by(my_data2, veg) %>%
    summarise(
        count = n(),
        mean = mean(Bothrops, na.rm = TRUE),
        sd = sd(Bothrops, na.rm = TRUE),
        median = median(Bothrops, na.rm = TRUE),
        IQR = IQR(Bothrops, na.rm = TRUE)
    )

ggboxplot(my_data2, x = "veg", y = "Bothrops",
          color = "veg", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
          ylab = "Acidentes ofídicos (Bothrops)", xlab = "Formação Vegetal")

ggline(my_data2, x = "veg", y = "Bothrops",
       add = c("mean_se", "jitter"),
       order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
       ylab = "Acidentes ofídicos (Bothrops)", xlab = "Formação Vegetal")

kruskal.test(Bothrops ~ veg, data = my_data2)

#Kruskal-Wallis rank sum test

#data:  Bothrops by veg
#Kruskal-Wallis chi-squared = 102.26, df = 4, p-value < 2.2e-16

pairwise.wilcox.test(my_data2$Bothrops, my_data2$veg,
                     p.adjust.method = "none")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction

#data:  my_data2$Bothrops and my_data2$veg

#                                 Estepe  Floresta Estacional Semidecidual Floresta Ombrofila Densa Floresta Ombrofila Mista
#Floresta Estacional Semidecidual 0.00067 -                                -                        -
#Floresta Ombrofila Densa         0.79720 0.00051                          -                        -
#Floresta Ombrofila Mista         0.20849 < 2e-16                          0.13398                  -
#Savana                           0.65868 5.4e-05                          0.31624                  0.56305
#P value adjustment method: none


############################Crotalus
my_data2

group_by(my_data2, veg) %>%
    summarise(
        count = n(),
        mean = mean(Crotalus, na.rm = TRUE),
        sd = sd(Crotalus, na.rm = TRUE),
        median = median(Crotalus, na.rm = TRUE),
        IQR = IQR(Crotalus, na.rm = TRUE)
    )

ggboxplot(my_data2, x = "veg", y = "Crotalus",
          color = "veg", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
          ylab = "Acidentes ofídicos (Crotalus)", xlab = "Formação Vegetal")

ggline(my_data2, x = "veg", y = "Crotalus",
       add = c("mean_se", "jitter"),
       order = c("Floresta Estacional Semidecidual", "Savana", "Floresta Ombrofila Mista", "Floresta Ombrofila Densa", "Estepe" ),
       ylab = "Acidentes ofídicos (Crotalus)", xlab = "Formação Vegetal")

kruskal.test(Crotalus ~ veg, data = my_data2)

#Kruskal-Wallis rank sum test

#data:  Crotalus by veg
#Kruskal-Wallis chi-squared = 12.056, df = 4, p-value = 0.01694

pairwise.wilcox.test(my_data2$Crotalus, my_data2$veg,
                     p.adjust.method = "none")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction

#data:  my_data2$Crotalus and my_data2$veg

#                                 Estepe Floresta Estacional Semidecidual Floresta Ombrofila Densa Floresta Ombrofila Mista
#Floresta Estacional Semidecidual 0.0530 -                                -                        -
#Floresta Ombrofila Densa         0.4993 0.9775                           -                        -
#Floresta Ombrofila Mista         0.0338 0.4136                           0.9365                   -
#Savana                           0.1953 0.0058                           0.3114                   0.0077

#P value adjustment method: none
