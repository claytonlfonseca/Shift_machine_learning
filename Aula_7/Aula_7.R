# Aula 7 - Clusteriza????o
# K-Means

m <- mtcars

# Tratamento de Dados

m$wt2 <- m$wt*6

plot(m$mpg~m$wt)

set.seed(33)

# k <- kmeans(m[,c("mpg","wt")], 4)
#k <- kmeans(m[,c("mpg","wt")], 5)
#k <- kmeans(m[,c("mpg","wt2")], 5)
k <- kmeans(m[,c("mpg","wt2")], 4)

# Visualiza????o com clusters com cor
plot(m$mpg~m$wt, col=k$cluster)

# Incluindo Cluster no DataSet
m$k <- k$cluster

# Tratando cluster como variavel categorica (Dummy)
m$k <- as.factor(k$cluster)



# Modelagem

mod <- lm(mpg~wt, data=m)
summary(mod)
mod2 <- lm(mpg~wt+k, data=m)
summary(mod2)

# Mais Algoritmos
t<-read.csv("train.csv")

media <- mean(t[!is.na(t$Age),]$Age)

t[is.na(t$Age) & t$title=="Mr",]$Age<-mediaMr
t[is.na(t$Age) & t$title=="Mrs",]$Age<-mediaMrs



#########Aula 6##############
# AD Titanic Sex + Age  + Pclass
# 0. Libs
install.packages("party")
library(party)
# 1. Carga de Dados
d <- read.csv("train.csv")
# 2. Tratamento de Variaveis
d$Pclass_c <- as.factor(d$Pclass)
#d$Fare_c <- as.factor(d$Fare)
# 3. Separa????o Treino e Teste
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
# 4. Modelagem
mod <- ctree(Survived~Sex+Pclass, data=treino)
plot(mod, type="simple")
p <- predict(mod, newdata=teste)
# 5. Analise de resultados
prev <- ifelse(p<.5,0,1)
# Matriz de Confus??o
table(prev, teste$Survived)



# Mais Algoritmos - Regress??o Logistica
# 0. Libs
install.packages("e1071")
library(e1071)
# 1. Carga de Dados
d <- read.csv("train.csv")
# 2. Tratamento de Variaveis
med <- mean(d[!is.na(d$Age)]$Age)
d[is.na(d$Age)]
#d$Fare_c <- as.factor(d$Fare)
# 3. Separa????o Treino e Teste
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
# 4. Modelagem
mod <- glm(Survived~Sex+Age+Pclass, data=treino, family = binomial())
p <- predict(mod, newdata=teste)
# 5. Analise de resultados
prev <- ifelse(p<.5,0,1)
# Matriz de Confus??o
table(prev, teste$Survived)


# Mais Algoritmos -  Random Forest Como Regressor
# 0. Libs
install.packages("randomForest")
library(randomForest)
# 1. Carga de Dados
t <- read.csv("train.csv")
# 2. Tratamento de Variaveis
med <- mean(t[!is.na(t$Age),]$Age)
d[is.na(d$Age),]$Age<-med
#d$Fare_c <- as.factor(d$Fare)
# 3. Separa????o Treino e Teste
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
# 4. Modelagem
mod <- randomForest(Survived~Sex+Age+Pclass, data=treino)
p <- predict(mod, newdata=teste)
# 5. Analise de resultados
prev <- ifelse(p<.5,0,1)
# Matriz de Confus??o
table(prev, teste$Survived)


# randomForest como Classificador
library(randomForest)
# 1. Carga de Dados
t <- read.csv("train.csv")
# 2. Tratamento de Variaveis
med <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age<-med
t$Survived_f <- as.factor(t$Survived)
t
#d$Fare_c <- as.factor(d$Fare)
# 3. Separa????o Treino e Teste
set.seed(33)
va<-sample(891)
treino<-t[va[1:600],]
treino
teste<-t[va[601:891],]
# 4. Modelagem
mod <- randomForest(Survived_f~Sex+Age+Pclass, data=treino)
p <- predict(mod, newdata=teste)
# 5. Analise de resultados
#prev <- ifelse(p<.5,0,1)
# Matriz de Confus??o
table(prev, teste$Survived)


# Outros Algoritmos VSM - Support Vector Machines
# SVM j?? instalado dentro do 1071
library(e1071)
# 1. Carga de Dados
t <- read.csv("train.csv")
# 2. Tratamento de Variaveis
med <- mean(t[!is.na(t$Age),]$Age)
t[is.na(t$Age),]$Age<-med
t$Survived_f <- as.factor(t$Survived)
#d$Fare_c <- as.factor(d$Fare)
# 3. Separa????o Treino e Teste
set.seed(33)
va<-sample(891)
treino<-t[va[1:600],]
teste<-t[va[601:891],]
# 4. Modelagem
mod <- svm(Survived~Sex+Age+Pclass, data=treino)
p <- predict(mod, newdata=teste)
#prev <- ifelse(p<.5,0,1)
prev <- ifelse(p<28,0,1)
# 5. Analise de resultados
#prev <- ifelse(p<.5,0,1)
# Matriz de Confus??o
table(prev, teste$Survived)
#prop.table(prev)
table(prev)
prop.table(table(prev))