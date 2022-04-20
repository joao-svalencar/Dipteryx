install.packages("nnet")
install.packages("GGally")
library(nnet)
library(GGally)

#simulando dados para analizar depois
total <- c(30,30,30,30,15,15,15,15,5,5,5,5)
restante <- c(5,4,3,8,5,4,2,7,2,0,2,0)
predadas <- c(10,15,20,8,5,4,2,2,1,3,1,4)
dispersas <- total - (restante + predadas)

head(fates)

#estacao <- as.factor(rep(c(rep("seca",2), rep("chuvosa",2)),3))
#densidade <- as.factor(total)

#preditores <- data.frame(estacao,densidade)
preditores <- fates[,c(2,3)]
head(preditores)
#sementes <- data.frame(restante,predadas,dispersas)
sementes <- fates[,c(4,5,6)]
head(sementes)

sementes <- as.matrix(sementes)

?multinom
#Rodando vários modelos com e sem as variaveis preditoras para comparar os modelos depois
modelo_sem_efeito <- multinom(sementes ~ 1, data = preditores) # sem nenhum efeito das preditoras
modelo_estacao <- multinom(sementes ~ season, data = preditores) #efeito apenas da estação
modelo_densidade <- multinom(sementes ~ density, data = preditores) #efeito apenas da densidade
modelo_estacao_densidade <- multinom(sementes ~ season + density, data = preditores) #efeito apenas da densidade e da estação
modelo_interacao <- multinom(sementes ~ season*density, data = preditores) #interação entre densidade e estação


###Abordagem frequentista
anova(modelo_sem_efeito,modelo_estacao,modelo_estacao_densidade,modelo_interacao)
#Densidade é significativo!


#Comparando modelos por AIC aqui é mais interessante pra ver qual modelo é melhor
install.packages("AICcmodavg")
library(AICcmodavg)

aictab(list(modelo_sem_efeito, modelo_estacao, modelo_densidade, modelo_estacao_densidade,modelo_interacao), second.ord = F,
       modnames = c("modelo_sem_efeito", "modelo_estacao","modelo_densidade","modelo_estacao_densidade","modelo_interacao"))
#Nesse caso tivemos efeito apenas da densidade.


summary(modelo_estacao_densidade)
# O modelo com menor AIC é sempre o melhor, e ele é considerado significativamente melhor que os outros se ele for
# mais de duas unidades de AIC menor que o outro (avaliamos isso de acordo com o Delta_AIC)


#Prediction
predict(modelo_estacao_densidade, preditores, type="prob")


#Misclassification error
cm <- table(predict(modelo_estacao_densidade), preditores$density)
cm

1-sum(diag(cm))/sum(cm)

?ggcoef
ggcoef(modelo_estacao_densidade)
