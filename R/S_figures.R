# reading libraries -------------------------------------------------------

library(plyr)
library(ggplot2)

# Initial-removed graph Fig. 1 --------------------------------------------
plot(log.removed~log.initial, data = ir, pch = 16, cex=0.7,
     ylab = "Number of removed endocarps (log)",
     xlab = "Initial number of endocarps (log)")

abline(ir.mod.dry, lty=3)
abline(ir.mod.wet, lty=1)
#abline(ir.mod, lty=3, col='red')
?legend
legend("topleft", c("Dry","Wet"), lty=c(3,1), cex=0.7)

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


# creating density x season x fates graph ---------------------------------
#GGally::ggcoef_multinom(interaction)

# preparing data ----------------------------------------------------------

p <- as.data.frame(unique(fitted(interaction)))
p$density <- cbind(rep(c("5", "15", "30"), times=2))
p$season <- cbind(rep(c("dry", "wet"), each=3))
p <- p[,c(6,5,1:4)]
rownames(p) <- NULL
write.csv(p, here::here("outputs", "tables", "pred_prob.csv"), row.names = FALSE)

pmelt <- melt(p, id.vars=c('season', 'density'), value.name='probability')
head(pmelt)

pmelt$density <- factor(pmelt$density, levels=c("5", "15", "30"))
str(pmelt)

pmelt$season <- factor(pmelt$season, levels=c("dry", "wet"))
str(pmelt)

# plot --------------------------------------------------------------------

ggplot(pmelt, aes(x = density, y = probability, shape = variable))+
  labs(x= "Seed Density", y= "Fates probability", shape='Seed fates')+
  geom_point(size=2) + facet_wrap(season ~.)+
  theme_bw()


ggplot(pmelt, aes(x = density, y = probability, colour = variable, shape=season))+
  labs(x='Seed density', y='Probability', shape='Season', colour='Seed fates')+
  geom_point(size=2)