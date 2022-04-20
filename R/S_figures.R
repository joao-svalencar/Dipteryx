# reading libraries -------------------------------------------------------

library(plyr)
library(ggplot2)

init_rem <- ggplot(data=ir)+
  geom_point(aes(x=log.initial, y=log.removed), size=0.8)+
  geom_line(data = fortify(ir.mod.dry), aes(x = log.initial, y = .fitted, linetype="Dry"))+
  geom_line(data = fortify(ir.mod.wet), aes(x = log.initial, y = .fitted, linetype="Wet"))+
  labs(x="Initial number of endocarps (log)", y="Number of removed endocarps (log)", linetype="Season")+
  theme_classic()+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8, margin = margin(t=0, r=0, b=0, l=0, unit="mm")),
        legend.position=c(.15, .85),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(8, "pt"),
        legend.margin = margin(t=0, r=-4, b=0, l=0),
        legend.box.margin=margin(-10,0 ,-10,-10))
init_rem
ggsave(paste(here::here(),"/outputs/figures/Fig 1.png",sep=""), init_rem, width = 79, height=70, units="mm", dpi =300)

# Dispersal distance bar plot ---------------------------------------------
# processing data: calculating necessary information ----------------------
head(dist)
str(dist)
dist$season <- as.factor(dist$season)
dist$density <- as.factor(dist$density)

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
  labs(x="Seed densities", y="Mean dispersal distance (m)", fill="Season")+
  theme_classic()+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8, margin = margin(t=0, r=0, b=0, l=0, unit="mm")),
        legend.position=c(.15, .85),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(8, "pt"),
        legend.margin = margin(t=0, r=-4, b=0, l=0),
        legend.box.margin=margin(-10,0 ,-10,-10))
dist_bar
ggsave(paste(here::here(),"/outputs/figures/Fig 2.png",sep=""), dist_bar, width = 79, height=70, units="mm", dpi =300)


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