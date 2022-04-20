# reading libraries -------------------------------------------------------
library(nnet)
library(AICcmodavg)
library(reshape2)
library(vegan)
library(GGally)
library(foreign)
# processing data: initial-removed relationship analysis ------------------

head(fates)

ir <- fates[,c(1:3,8)] #new dataset with columns remnant, density, season and removed
head(ir)

ir$initial.1 <- ir$density + 1
ir$removed.1 <- ir$removed + 1
head(ir)

ir$log.initial <- log10(ir$initial.1)
ir$log.removed <- log10(ir$removed.1)

str(ir)
# initial-removed relationship analysis test ------------------------------

ir.mod.dry <- lm(log.removed~log.initial, data = ir[ir$season == "dry",])
summary(ir.mod.dry)

ir.mod.wet <- lm(log.removed~log.initial, data = ir[ir$season == "wet",])
summary(ir.mod.wet)

ir.mod <- lm(log.removed~log.initial*season, data = ir)
summary(ir.mod)

# processing data: seed fates multinomial models --------------------------

fates$density <- as.factor(fates$density)
fates$season <- as.factor(fates$season)
fates$remnant <- as.factor(fates$remnant)

preds <- fates[,c(2,3)] #selecting only the columns density and season to the object predictors
head(preds) #checking the head of the object

seeds <- fates[, c(4:7)] #selecting only the columns intact, preyed and dispersed to the object seeds
head(seeds)

seeds <- as.matrix(seeds)

# seed fates multinomial models -------------------------------------------

null <- nnet::multinom(seeds ~ 1, data = preds) # sem nenhum efeito das preditoras
season <- nnet::multinom(seeds ~ season, data = preds) #efeito apenas da estação
density <- nnet::multinom(seeds ~ density, data = preds) #efeito apenas da densidade
season_density <- nnet::multinom(seeds ~ season + density, data = preds) #efeito apenas da densidade e da estação
interaction <- nnet::multinom(seeds ~ season*density, data = preds) #interação entre densidade e estação

# Comparando modelos por AIC aqui é mais interessante pra ver qual modelo é melhor

mod.sel <- AICcmodavg::aictab(list(null, season, density, season_density, interaction), second.ord = F,
                  modnames = c("null", "season","density","season_density","interaction"))

mod.sel

capture.output(mod.sel, file = here::here("outputs", "tables", "AIC_table.txt"))

# O modelo com menor AIC é sempre o melhor, e ele é considerado significativamente melhor que os outros se ele for
# mais de duas unidades de AIC menor que o outro (avaliamos isso de acordo com o Delta_AIC)

summary(interaction)
capture.output(summary(interaction), file = here::here("outputs", "tables", "model_summary.txt"))

############################################################################################################
############################################################################################################
# DISPERSAL DISTANCE
############################################################################################################
############################################################################################################

head(dist)
str(dist)
dist$season <- as.factor(dist$season)
dist$density <- as.factor(dist$density)
#dist$distance.1 <- dist$distance +1
#dist$log.dist <- log10(dist$distance.1)

### models ###
?lm
lm <- lm(log.dist~season*density, data=dist)
summary(lm)

lm2 <- lm(distance~season+density, data= dist)
summary(lm2)

anova(lm, lm2) #não diferem, mantenho o mais simples

lm3 <- lm(distance~density, data= dist)
summary(lm3)
anova(lm2, lm3)

log10(dist$distance)
lm4 <- lm(l(distance)~season, data= dist)
summary(lm4)
anova(lm3, lm4)


