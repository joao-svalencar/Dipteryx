# reading libraries -------------------------------------------------------

library(here)
library(nnet)
library(AICcmodavg)

library(reshape2)
library(vegan)
library(GGally)

# loading data ------------------------------------------------------------

fates <- read.csv(here::here("data", "fates.csv"))
head(fates)
str(fates)

dist <- read.csv(here::here("data", "dist.csv"))
head(dist)
str(dist)

# processing data: initial-removed relationship analysis ------------------

fates$removed <- fates$density - fates$intact
head(fates)
fates

ir <- fates[,c(1:3,8)]
head(ir)

ir$initial <- ir$density + 1
ir$removed.1 <- ir$removed + 1
head(ir)

ir$log.initial <- log10(ir$initial)
ir$log.removed <- log10(ir$removed.1)
head(ir)

ir <- ir[!(ir$season == "dry" & ir$remnant == "bjad"),]

# initial-removed relationship analysis test ------------------------------

ir.mod <- lm(log.removed~log.initial, data = ir)
summary(ir.mod)

ir.mod.dry <- lm(log.removed~log.initial, data = ir[ir$season == "dry",])
summary(ir.mod.dry)

ir.mod.wet <- lm(log.removed~log.initial, data = ir[ir$season == "wet",])
summary(ir.mod.wet)


# another way to do the initial removed test ------------------------------

fates <- fates[!(fates$season == "dry" & fates$remnant == "bjad"),]

fates$removed <- fates$density - fates$intact

fates$bin <- fates$removed
fates$bin[fates$bin>0] <- 1

head(fates)

#fates$prop <- cbind(sucess = fates$removed, fail = fates$intact)

fates$density <- as.factor(fates$density)
fates$season <- as.factor(fates$season)
fates$remnant <- as.factor(fates$remnant)
str(fates)


# creating full model -----------------------------------------------------

ir.mod <- glm(bin~density + season + density:season, family = binomial, data = fates)


# model simplificantion: removing interaction -----------------------------

ir.mod.1 <- glm(bin~density + season, family = binomial, data = fates)


# comparing models: with interaction versus without interaction -----------

anova(ir.mod.1, ir.mod, test= "Chisq") # keep the simplest (ir.mod.1)


# model simplification: removing the effect of density --------------------

ir.mod.2 <- glm(bin~season, family = binomial, data = fates)


# comparing models: density + season versus only season -------------------

anova(ir.mod.2, ir.mod.1, test = 'Chisq') # keep the most complex (ir.mod.1)


# model simplification: removing the effect of seasons --------------------

ir.mod.3 <- glm(bin~density, family = binomial, data = fates)


# comparing models: density + season versus only density ------------------

anova(ir.mod.3, ir.mod.1, test = 'Chisq') # keep the simplest (ir.mod.3)

summary(ir.mod.3)


predicted <- predict(ir.mod.3, type = "response")
unique(predicted)

plot(predicted)

plot(ir.mod)
# processing data: seed fates multinomial models --------------------------

fates$density <- as.factor(fates$density)
fates$season <- as.factor(fates$season)
fates$remnant <- as.factor(fates$remnant)

predictors <- fates[,c(2,3)] #selecting only the columns density and season to the object predictors
head(predictors) #checking the head of the object

seeds <- fates[, c(4:7)] #selecting only the columns intact, preyed and dispersed to the object seeds
head(seeds)

seeds <- as.matrix(seeds)

# seed fates multinomial models -------------------------------------------

?multinom

null <- nnet::multinom(seeds ~ 1, data = preds) # sem nenhum efeito das preditoras
season <- nnet::multinom(seeds ~ season, data = preds) #efeito apenas da estação
density <- nnet::multinom(seeds ~ density, data = preds) #efeito apenas da densidade
season_density <- nnet::multinom(seeds ~ season + density, data = preds) #efeito apenas da densidade e da estação
interaction <- nnet::multinom(seeds ~ season*density, data = preds) #interação entre densidade e estação

# Comparando modelos por AIC aqui é mais interessante pra ver qual modelo é melhor

AICcmodavg::aictab(list(null, season, density, season_density, interaction), second.ord = F,
                  modnames = c("null", "season","density","season_density","interaction"))

# Nesse caso tivemos efeito apenas da densidade.

summary(modelo_estacao_densidade)
# O modelo com menor AIC é sempre o melhor, e ele é considerado significativamente melhor que os outros se ele for
# mais de duas unidades de AIC menor que o outro (avaliamos isso de acordo com o Delta_AIC)

############################################################################################################
############################################################################################################
# ANALYSIS Two-Way
############################################################################################################
############################################################################################################
m.per <- fates[,c(4,5,6)] #matrix with seed fates proportions
m.fac <- fates[,c(2,3)] #matrix with categorical variables

m.per <- asin(sqrt(m.per)) #square root arc-sin transformation

#two-way permanova with adonis from package vegan

twperm <- adonis(formula = m.per ~ season * density, data = m.fac, permutations = 999, method = "euclidean", strata = NULL)
twperm
twperm$coefficients

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


