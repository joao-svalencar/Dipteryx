# reading libraries -------------------------------------------------------

library(plyr)
library(ggplot2)

# Initial-removed graph Fig. 1 --------------------------------------------
plot(log.removed~log.initial, data = ir, pch = 16,
     ylab = "Log nº of removed endocarps",
     xlab = "Log initial nº of endocarps")

abline(ir.mod.dry, col = "black")
abline(ir.mod.wet, col = "red")

# Dispersal distance bar plot ---------------------------------------------
# processing data: calculating necessary information ----------------------
head(dist)
str(dist)
dist$season <- as.factor(dist$season)
dist$density <- as.factor(dist$density)

?ddply
dist_sum <- plyr::ddply(dist, c("density", "season"), summarise,
                        N    = length(distance),
                        mean = mean(distance),
                        sd   = sd(distance),
                        se   = sd / sqrt(N))

dist_sum

# creating dispersal distance graph ---------------------------------------
dist_bar <- ggplot(dist_sum, aes(x=density, y=mean, fill=season)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", size=.5, width=0.7) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                size=.5, width=.1,position=position_dodge(0.7))+
  scale_fill_manual(values=c("#FFFFFF", "#CCCCCC"))+
  scale_y_continuous(expand=c(0,0), limits=c(0,8))+
  theme_classic(base_size = 18)+
  xlab("Seed densities")+
  ylab("Mean dispersal distance (m)")

dist_bar

# WARNING: ABOVE IS WORKING, BELOW IS TESTING --------------------------------------
# kind of working:
?ggplot
fates <- ggplot(p, aes(x=density, y=intact)) +
  labs(x= "Seed Density", y= "Fates probability" )+
  geom_point(size=2)+
  scale_y_continuous(limits = c(0, 0.7))+
  scale_x_discrete(limits=c("5","15","30"))+
  facet_wrap(~season)

fates + geom_point(aes(x=density, y=preyed), col="red", size=2) +
  geom_point(aes(x=density, y=dispersed), col="orange", size=2) +
  geom_point(aes(x=density, y=lost), col="blue", size=2)

#trying some variations
seed.fates <- ggplot(p, aes(x=density, y=intact, shape=season)) +
  labs(x= "Seed Density", y= "Fates probability" )+
  geom_point(size=2)+
  scale_y_continuous(limits = c(0, 0.7))+
  scale_x_discrete(limits=c("5","15","30"))+
  geom_point(aes(x=density, y=preyed), col="red", size=2) +
  geom_point(aes(x=density, y=dispersed), col="orange", size=2) +
  geom_point(aes(x=density, y=lost), col="blue", size=2)

seed.fates

str(p)
p$season <- factor(p$season)
p$season <- as.character(p$density)

############################################################################################################
############################################################################################################
# calculating seed fates proportion
############################################################################################################
############################################################################################################

fates$intact[fates$density=="5"] <- fates$intact[fates$density=="5"]/5
fates$intact[fates$density=="15"] <- fates$intact[fates$density=="15"]/15
fates$intact[fates$density=="30"] <- fates$intact[fates$density=="30"]/30


fates$preyed[fates$density=="5"] <- fates$preyed[fates$density=="5"]/5
fates$preyed[fates$density=="15"] <- fates$preyed[fates$density=="15"]/15
fates$preyed[fates$density=="30"] <- fates$preyed[fates$density=="30"]/30

fates$dispersed[fates$density=="5"] <- fates$dispersed[fates$density=="5"]/5
fates$dispersed[fates$density=="15"] <- fates$dispersed[fates$density=="15"]/15
fates$dispersed[fates$density=="30"] <- fates$dispersed[fates$density=="30"]/30

head(fates)

fates.m <- melt(fates[-1], id.var=c("density","season"))
fates.m$season = factor(fates.m$season, levels=c("dry", "wet"))

head(fates.m)

fates.m$value <- asin(sqrt(fates.m$value))

############################################################################################################
############################################################################################################
# GRAPHS
############################################################################################################
############################################################################################################

fates.m = melt(fates[-1], id.var=c("density","season"))
fates.m$season = factor(fates.m$season, levels=c("dry", "wet"))

# If you want the two levels of event plotted side by side
test <- ggplot(fates.m, aes(density, value, colour=season)) +
  labs(x= "Seed Density", y= "Percentage of seed fates" )+
  facet_grid(. ~ variable) +
  scale_y_continuous(limits = c(0, 1.25))+
  geom_boxplot(width=0.7)
test

test <- ggplot(fates.m, aes(variable, value, colour=season)) +
  labs(x= "Seed fates", y= "Percentage")+
  facet_grid(. ~ density) +
  scale_y_continuous(limits = c(0, 1.25))+
  geom_boxplot(width=0.7)
test

fates_hs <- ggplot(fates, aes(x=density)) +
  labs(x= "Seed Density", y= NULL )+
  geom_bar()+
  scale_y_continuous(limits = c(0, 35))+
  facet_wrap(~season)
?geom_histogram
fates_hs

############################################################################################################
############################################################################################################
# AGGREGATED
############################################################################################################
############################################################################################################

fates2 <- read.csv("fates.csv")
head(fates2)

fates2 <- fates2[fates2$remnant!="bjad",]

fates2.ag <- aggregate(cbind(intact=fates2[-1]$intact, preyed=fates2[-1]$preyed, dispersed=fates2[-1]$dispersed), 
                       by=list(density=fates2[-1]$density, season=fates2[-1]$season),
                       FUN=sum)
fates2.ag

fates.agm <- melt(fates2.ag, id.var=c("density","season"))
fates.agm$season <- factor(fates.agm$season, levels=c("dry", "wet"))

fates.agm

fates_hs <- ggplot(fates.agm, aes(x=density, fill=season)) +
  geom_bar()+
  scale_y_continuous(limits = c(0, 30))+
  facet_grid(.~variable)

fates_hs

fates_hs <- ggplot(fates.agm, aes(x=density, y=value), fill=season) +
  labs(x= NULL, y= NULL )+
  geom_bar(aes(y=value), stat="identity", position="identity",  binwidth=.5)+
  scale_y_continuous(limits = c(0, 500))+
  facet_grid(.~variable)

fates_hs

############################################################################################################
############################################################################################################
# PROPORTION AGGREGATED
############################################################################################################
############################################################################################################

fates2.ag$intact[fates2.ag$density=="5"] <- fates2.ag$intact[fates2.ag$density=="5"]/150
fates2.ag$preyed[fates2.ag$density=="5"] <- fates2.ag$preyed[fates2.ag$density=="5"]/150
fates2.ag$dispersed[fates2.ag$density=="5"] <- fates2.ag$dispersed[fates2.ag$density=="5"]/150

fates2.ag$intact[fates2.ag$density=="15"] <- fates2.ag$intact[fates2.ag$density=="15"]/450
fates2.ag$preyed[fates2.ag$density=="15"] <- fates2.ag$preyed[fates2.ag$density=="15"]/450
fates2.ag$dispersed[fates2.ag$density=="15"] <- fates2.ag$dispersed[fates2.ag$density=="15"]/450

fates2.ag$intact[fates2.ag$density=="30"] <- fates2.ag$intact[fates2.ag$density=="30"]/900
fates2.ag$preyed[fates2.ag$density=="30"] <- fates2.ag$preyed[fates2.ag$density=="30"]/900
fates2.ag$dispersed[fates2.ag$density=="30"] <- fates2.ag$dispersed[fates2.ag$density=="30"]/900

fates2.ag

fates_hs <- ggplot(fates2.ag, aes(x=density)) +
  labs(x= NULL, y= NULL )+
  geom_bar()+
  scale_y_continuous(limits = c(0, 1.2))+
  facet_wrap(~season)

fates_hs

x <- 1:10
y <- 1:10
plot(x,y)
?clip
clip(1,10, -100, 100)
abline(lm(y~x))
