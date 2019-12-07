### Aula 6
# Arvores de Decisão

d <- read.csv("train.csv")

install.packages("party")
library(party)

# modelo de decision tree (Arvore binária)
mod <- ctree(Survived~Sex, data=d)

# Para este modelo analisaremos R- Quadrado, construção da Arvore

# Visualozar a arvore
plot(mod, type="simple")

# modelo de decision tree, com duas variaveis
mod <- ctree(Survived~Sex+Pclass, data=d)
plot(mod, type="simple")

# modelo de decision tree, com mais variaveis
mod <- ctree(Survived~Sex+Pclass+Age, data=d)
plot(mod, type="simple")

# Percebenos que a inclusão da idade explica muito melhor a probabilidade dos homens sobreviverem, porém não influencia para as mulheres

# verificação quantidade Mulheres por idade
sqldf("select Age, count() from d where Sex='female' group by Age")

sqldf("select Fare, count() from d group by Fare order by 1 desc")

## Testando modelo para verificar o erro de previsão

## Pipeline M.L. Com Arvore de Decisão

d <- read.csv("train.csv")
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
mod <- ctree(Survived~Sex+Pclass, data=treino)
plot(mod, type="simple")
# Previsão
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)
# Comparação de precisões e Dados Reais
cbind(prev, teste$Survived)
cbind(prev, teste$Survived, ifelse(prev==teste$Survived,1,0))
# Contagem de Acertos
sum(prev==teste$Survived,1,0)
# Taxa de Acertos
sum(prev==teste$Survived,1,0)/291
# Matriz de Confusão
table(prev, teste$Survived)

## Agora com Idade, segundo modelo
## AD Titanic: Sex+Pclass+Age
d <- read.csv("train.csv")
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
#mod <- ctree(Survived~Sex+Pclass+Age, data=treino)
mod <- ctree(Survived~Sex+Age, data=treino)
plot(mod, type="simple")
# Previsão
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)
# Comparação de precisões e Dados Reais
cbind(prev, teste$Survived)
cbind(prev, teste$Survived, ifelse(prev==teste$Survived,1,0))
# Contagem de Acertos
sum(prev==teste$Survived,1,0)
# Taxa de Acertos
sum(prev==teste$Survived,1,0)/291
# Matriz de Confusão
table(prev, teste$Survived)
# Na matriz de confusão os valores respeitam seu eixo, que pode ser definido por ordenação diferente
# Na matriz acima os valores navegam verticalmente, na matriz abaixo navegam horizontalmente
table(teste$Survived, prev)


# Variaveis Categoricos
v1<-1:6
v2<-factor(c(1,2,3,3,3,1))
cat<-data.frame(cbind(v1,v2))
cat$v2 <- as.factor(cat$v2)
#append de linhas
rbind(cat,c(7,4))

levels(cat$v2)<-c(levels(cat$v2), "4")
levels((cat$v2))
rbind(cat,c(7,4))



# AD Titanic Sex + Age 
# 0. Libs
library(party)
# 1. Carga de Dados
d <- read.csv("train.csv")
# 2. Tratamento de Variaveis
d$Pclass_c <- as.factor(d$Pclass)
#d$Fare_c <- as.factor(d$Fare)
# 3. Separação Treino e Teste
set.seed(33)
va<-sample(891)
treino<-d[va[1:600],]
teste<-d[va[601:891],]
# 4. Modelagem
mod <- ctree(Survived~Sex+Pclass_c, data=treino)
plot(mod, type="simple")
p <- predict(mod, newdata=teste)
# 5. Analise de resultados
prev <- ifelse(p<.5,0,1)
# Matriz de Confusão
table(prev, teste$Survived)

