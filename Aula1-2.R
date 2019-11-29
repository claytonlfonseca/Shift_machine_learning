# Comandos Básicos do R
a <- 23
b <- a + 1
# Carregar um DataFrame para uma variável
m <- mtcars

# Amostra das primeiras 6 linhas
head(m)
View(head(m))

# Exportar Dados
write.csv(m, "mtcars.csv")

# Configurar Pasta base para projeto
setwd("~/Codes/personal_github/Shift_machine_learning")

# Importar Dados
m <- read.csv("mtcars.csv")

# Package especifico para buscarmos um DataSet
# cran é o repositório de packages do R, http://cran.r-project.org
# ggplot2 = https://cran.r-project.org/web/packages/ggplot2/index.html

install.packages("ggplot2")

# Carregar uma package previamente instalada
library(ggplot2)

# Amostragem
d <- diamonds

# População 53940 Diamantes
# Geração Amostra de 3000 linhas
a1 <- d[1:3000,]
#[antes da virgula são as linhas, depois da virgulas colunas]

# Estatisticas descritivas População
# Média da população
mean(d$price)

# Mediana de Price dentro da população
median(d$price)

# Podemos ver se existem valores extremos, pois a média é consideravelmente maior que a mediana

# Desvio Padrão
sd(d$price)

# Estatisitcas descritivas  da Amostra
# Média
mean(a1$price)

# Mediana
median(a1$price)

# Desvio Padrão
sd(a1$price)

# Percebemos que a amostra não conta a mesma história que a população

# Histograma
# Analise de Distribuição
# população
hist(d$price)
# Amostra 1
hist(a1$price)

# Geração de nova amostra de 3000 linhas
a2 <- d[3001:6000,]

# Estatisticas descritivas
# Média
mean(a2$price)
# Mediana
median(a2$price)
# Desvio Padrao
sd(a2$price)
# Histograma
hist(a2$price)

# Combinar visualizaçõees

# Janela Grafica 2x3 e 2x2
par(mfrow=c(2,2))
hist(d$price)
hist(a1$price)
hist(a2$price)

# Gerar um numero aleatório
set.seed(33) # Setar a semente de comparação, versão especifica de amostra aleatório
sample(3) # amostra propriamente dita

# Gerar Amostra aleatória da população
set.seed(33)
va <- sample(nrow(d)) # nrow é o numero de linhas da população

# nesta ação embaralhamos a ordem as linhas dentro da população

# Amostra 3 com linhas embaralhadas

a3 <- d[va[1:3000],]

# Estatisticas descritivas
# Média
mean(a3$price)
# Mediana
median(a3$price)
# Desvio Padrao
sd(a3$price)
# Histograma
hist(a3$price)

# Agora as variaveis estatisticas descritivas estão próximas, o histograma mostra bem isso.

# Estatística descritiva
# Quartis
summary(d)
summary(a3)

summary(d$price)
summary(a3$price)


# Boxplot
par(mfrow=c(2,2))
boxplot(d$price)
boxplot(a1$price)
boxplot(a2$price)
boxplot(a3$price)


# Estatísticas de mais de uma variável

a <- anscombe

mean(a$x1)
mean(a$x2)
mean(a$x3)
mean(a$x4)

mean(a$y1)
mean(a$y2)
mean(a$y3)
mean(a$y4)

sd(a$x1)
sd(a$x2)
sd(a$x3)
sd(a$x4)

sd(a$y1)
sd(a$y2)
sd(a$y3)
sd(a$y4)

# Calculo de correlação linear do anscombe
cor(a$y1, a$x1)
cor(a$y2, a$x2)
cor(a$y3, a$x3)
cor(a$y4, a$x4)

par(mfrow=c(2,2))
plot(a$y1, a$x1)
plot(a$y2, a$x2)
plot(a$y3, a$x3)
plot(a$y4, a$x4)



# Gráfico de dispersão - Scatterplot
# procurando comportamento entre duas variaveis

plot(m$mpg~m$wt)

# coeficiente de correlação linear
cor(m$mpg, m$wt)

# Matriz de Correlação, todas correlações possíveis
m <- mtcars
cor(m)

# Exercício
install.packages("swirl")
library("swirl")
# utilizar Swirl - livro da Avril
# Realizar primeiro programa = R
swirl()