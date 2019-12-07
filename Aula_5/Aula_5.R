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
