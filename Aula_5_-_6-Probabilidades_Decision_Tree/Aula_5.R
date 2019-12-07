# Aula 5
m <- mtcars

#variaveis dummy no R
install.packages("dummies")
library(dummies)

#preparando variaveis
m<-cbind(m,dummy(m$cyl))

# amostra aleatória
set.seed(33)

treino<-m[va[1:24],]

teste<-m[va[25:32],]

#coeficiente de correlação linear, para selecionar variaveis
plot(m$mpg~m$wt)

#modelo de regressão linear
#mod<-lm(mpg~wt,data=treino)
#mod<-lm(mpg~log(wt),data=treino)
#mod<-lm(mpg~poly(wt,2),data=treino)
#mod<-lm(mpg~poly(wt,30),data=treino)
#mod<-(mpg~wt+cyl,data=treino)
#incluindo variavel cyl como variavel categorica
#mod<-lm(mpg~wt+as.factor(cyl),data=treino)
#incluindo variavel cyl como variavel categorica sem o 6 para o cyl
#mod<-lm(mpg~wt+as.factor(cyl),data=treino)
#com variaveis dummie criadas pelo package dummies
mod<-lm(mpg~wt+as.factor(cyl),data=treino)


summary(mod)

#Previsão a partir dos dados de tes
p<-predict(mod, newdata=teste)

#Visualização previsão vs real
cbind(p,teste$mpg)

#calculo erro de previsão - diferença quadrado = sse
cbind(p,teste$mpg, p-teste$mpg)
sse<-sum((p-teste$mpg)^2)

## até aqui!!!!!!!

# Ttansformação log(logaritmica)
# obj: linearizar variavel wt

#correlação:
cor(m$mpg, m$wt)
#correlaçõa com log
cor(m$mpg, log(m$wt))

plot(m$mpg~log(m$wt))


#variaveis dummy no R
install.packages("dummies")
library(dummies)

#preparando variaveis
m<-cbind(m,dummy(m$cyl))

##################################
#Probabilidades

# Titanic com probabilistica

d <- read.csv("train.csv")

##Contagens

##quantos sobreviventes
#Contagem de linhas com condição sobre coluna
sobr<- nrow(d[d$Survived==1,])

#probabilidade de sobrevivencia
sobr/nrow(d)

##Quantas mulheres - probabilidade
mul<- nrow(d[d$Sex=="female",])

#probabilidade de sobrevivencia
mul/nrow(d)


# Package SQL
install.packages("sqldf")
library(sqldf)
df <- sqldf("select Survived, count() from d group by Survived")


#Função Table
table(d$Survived)

#Provabilidade 
##Quantas mulheres sobreviventes
mul_viva<- nrow(d[d$Sex=="female"&d$Survived==1,])

#probabilidade de sobrevivencia
mul_viva/nrow(d)


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

sqldf("select Fare, count() from d group by Fare order by 2 desc")
#d2 <- sqldf

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

