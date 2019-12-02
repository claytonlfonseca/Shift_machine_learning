#Carga da Base
pib <- read.csv2("PIB.csv")

#Dafataframes de treino e teste
treino <- pib[1:132,]
teste <- pib[133:138,]

#Regressão Linear Simples
mod<-lm(PIB~BRL, data = treino)

#estatisticas do modelo
summary(mod)

#Previsão 
p<-predict(mod,newdata = teste)

#comparação previsão vs dado real
cbind(p,teste$PIB)

#comparação com magnitude
cbind(p, teste$PIB, p-teste$PIB)

#somatorio dos erros quadrados, para remover negativos
#sse = sum of square errors
sse<-sum((p-teste$PIB)^2)


#Regressão Linear Multivariado
mod<-lm(PIB~BRL+BRP, data = treino)

#estatisticas do modelo
summary(mod)

#Previsão 
p<-predict(mod,newdata = teste)

#comparação previsão vs dado real
cbind(p,teste$PIB)

#comparação com magnitude
cbind(p, teste$PIB, p-teste$PIB)

#somatorio dos erros quadrados, para remover negativos
#sse = sum of square errors
sse<-sum((p-teste$PIB)^2)



#Modelo Autoregressivo
mod<-lm(PIB~PIBi1+PIBi2, data = treino)

#estatisticas do modelo
summary(mod)

#Previsão 
p<-predict(mod,newdata = teste)

#comparação previsão vs dado real
cbind(p,teste$PIB)

#comparação com magnitude
cbind(p, teste$PIB, p-teste$PIB)

#somatorio dos erros quadrados, para remover negativos
#sse = sum of square errors
sse<-sum((p-teste$PIB)^2)


#Multivariado com Saz
mod<-lm(PIB~BRP+BRL+D2+D4+D5+D6+D7+D8+D9+D11, data = treino)

#estatisticas do modelo
summary(mod)

#Previsão 
p<-predict(mod,newdata = teste)

#comparação previsão vs dado real
cbind(p,teste$PIB)

#comparação com magnitude
cbind(p, teste$PIB, p-teste$PIB)

#somatorio dos erros quadrados, para remover negativos
#sse = sum of square errors
sse<-sum((p-teste$PIB)^2)


#Exercicio próxima semana:
#https://www.kaggle.com/c/titanic 

