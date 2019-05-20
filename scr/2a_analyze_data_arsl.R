# Face Morph graphs (Arousal)
# KLS and EAL 4/26/19, SA 5/15/19
rm(list=ls(all=TRUE))

library(ggplot2); library(reshape2); library(plyr)

setwd("~/Dropbox (MCAB Lab)/MCAB/Drafts/facemorphs")
source('scr/SummarySE.R')
#===============
# arousal
#===============
f <- read.csv('data/all_faces_ratings.csv')
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
fa2 <- summarySE(fa1, 'rating', groupvars=c('agegrp', 'emotion', 'level'))
fa3 <- summarySE(fa1, 'rating', groupvars=c('agegrp', 'emotion'))
fa4 <- summarySE(fa1, 'rating', groupvars = c('agegrp' , 'level' ))


# graph of emotion x age interaction
ea = ggplot(fa3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
#+ ylab ("Arousal Rating")


# graph of level x age interaction
mal = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")
# 
# graph of level x age interaction without legend
ma = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")

# graph of emotion x mag x age interaction
ema = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

emal = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Magnitude of Expression") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

#+ ylab ("Arousal Rating") 
ggsave(ema, filename = "a_ema.png", width = 11, height = 7, units = "in")
ggsave(ma, filename = "a_ma.png", width = 6, height = 7, units = "in")
ggsave(ea, filename = "a_ea.png",  width = 6, height = 7, units = "in")
ggsave(mal, filename = "a_mal.png",  width = 6, height = 7, units = "in", bg="transparent")
ggsave(emal, filename = "a_emal.png",  width = 11, height = 7, units = "in", bg="transparent")


ema
ma
ea

