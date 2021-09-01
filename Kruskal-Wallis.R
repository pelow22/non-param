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
                     p.adjust.method = "BH")

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction

#data:  my_data2$Total and my_data2$veg

#Estepe  Floresta Estacional Semidecidual Floresta Ombrofila Densa
#Floresta Estacional Semidecidual 0.00031 -                                -
#Floresta Ombrofila Densa         0.89810 0.04082                          -
#Floresta Ombrofila Mista         0.06222 < 2e-16                          0.45455
#Savana                           0.08385 0.04082                          0.45455
#Floresta Ombrofila Mista
#Floresta Estacional Semidecidual -
#Floresta Ombrofila Densa         -
#Floresta Ombrofila Mista         -
#Savana                           0.74104

#P value adjustment method: BH

