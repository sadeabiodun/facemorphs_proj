# Face Morph graphs (Arousal)
# KLS and EAL 4/26/19, SA 5/15/19
#Updated 5/19/20

library(ggplot2); library(reshape2); library(plyr); library(dplyr); library(plotly); library(wesanderson)

source('scr/SummarySE2.R')
#===============
# arousal
#===============
f <- read.csv('data/ave_faces_ratings.csv')
fa <- f[which(f$domain == 'arsl'),]

# remove age and domain variable (missing data)
fa$domain <- NULL 
fa$age <- NULL

# reorder age variable
fa$agegrp <- relevel(fa$agegrp, 'Younger')

# remove people with incomplete data
fa <- fa[complete.cases(fa),]

# change from wide to long format
fa1 <- melt(fa, id.vars=c('subnum','agegrp'), value.name = 'rating')

# make new variable for emotion
fa1$emotion <- as.factor(t(as.data.frame(strsplit(as.character(fa1$variable), '[.]')))[,1])
fa1$emotion <- revalue(fa1$emotion, c('a'= 'Angry', 's' = 'Sad', 'h' = 'Happy'))
fa1$emotion <- relevel(fa1$emotion, 'Sad')
fa1$emotion <- relevel(fa1$emotion, 'Happy')


# make new variable for level
fa1$level <- as.factor(t(as.data.frame(strsplit(as.character(fa1$variable), '[.]')))[,2])
fa1$level <- relevel(fa1$level, 'med')
fa1$level <- relevel(fa1$level, 'low')
fa1$level <- revalue(fa1$level, c('low'= 'Low', 'med' = 'Med', 'full' = 'Full'))

fa1$variable <- NULL


# create a summary table for graphing
fa2 <- summarySE2(fa1, measurevar = 'rating', groupvars=c('agegrp', 'emotion', 'level'))
fa3 <- summarySE2(fa1, 'rating', groupvars=c('agegrp', 'emotion'))
fa4 <- summarySE2(fa1, 'rating', groupvars = c('agegrp' , 'level' ))


# graph of emotion x age interaction
emo_age_leg_arsl = ggplot(fa3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) #ylab ("Arousal Rating")

######

# graph of level x age interaction
mag_age_leg_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")
mag_age_leg_arsl

# graph of level x age interaction (without legend)
mag_age_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")

######

# graph of emotion x mag x age interaction 
emo_mag_age_leg_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + ylab("Arousal Rating") + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + scale_fill_manual(values =wes_palette("Chevalier1"))

emo_mag_age_leg_arsl

# graph of emotion x mag x age interaction (no legend)
emo_mag_age_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

#+ ylab ("Arousal Rating") 
#ggsave(emo_age_leg_arsl, filename = "plots/emo_age_legend_arsl.png", width = 11, height = 7, units = "in")
#ggsave(mag_age_leg_arsl, filename = "plots/mag_age_legend_arsl.png", width = 6, height = 7, units = "in")
#ggsave(mag_age_arsl, filename = "plots/mag_age_arsl.png",  width = 6, height = 7, units = "in")
#ggsave(emo_mag_age_arsl, filename = "plots/emo_mag_age_arsl.png",  width = 6, height = 7, units = "in", bg="transparent")
ggsave(emo_mag_age_leg_arsl, filename = "plots/emo_mag_age_legend_arsl.png",  width = 11, height = 7, units = "in", bg="transparent")


ema
ma
ea

