# Face Morph graphs (Valence)
# KLS and EAL 4/26/19, SA 5/15/19
rm(list=ls(all=TRUE))

library(ggplot2); library(reshape2); library(plyr)

setwd("~/Dropbox (MCAB Lab)/MCAB/Drafts/facemorphs")
source('scr/SummarySE.R')

#===============
# valence 
#===============

f <- read.csv('data/all_faces_ratings.csv')
fv <- f[which(f$domain == 'vln'),]

# remove age and domain variable (missing data)
fv$domain <- NULL 
fv$age <- NULL

#get summary stats for demographics 
summary(fv)

# reorder age variable
fv$agegrp <- relevel(fv$agegrp, 'Younger')

# remove people with incomplete data
fv <- fv[complete.cases(fv),]

# change from wide to long format
fv1 <- melt(fv, id.vars = c('subnum','agegrp'), value.name = 'rating')

# make new variable for emotion
fv1$emotion <- as.factor(t(as.data.frame(strsplit(as.character(fv1$variable), '[.]')))[,1])
fv1$emotion <- revalue(fv1$emotion, c('a'= 'Angry', 's' = 'Sad', 'h' = 'Happy'))
fv1$emotion <- relevel(fv1$emotion, 'Sad')
fv1$emotion <- relevel(fv1$emotion, 'Happy')


# make new variable for level
fv1$level <- as.factor(t(as.data.frame(strsplit(as.character(fv1$variable), '[.]')))[,2])
fv1$level <- relevel(fv1$level, 'med')
fv1$level <- relevel(fv1$level, 'low')
fv1$level <- revalue(fv1$level, c('low'= 'Low', 'med' = 'Med', 'full' = 'Full'))


# delete variable column
fv1$variable <- NULL

# create a summary table for graphing
fv2 <- summarySE(fv1, 'rating', groupvars=c('agegrp', 'emotion', 'level'))
fv3 <- summarySE(fv1, 'rating', groupvars=c('agegrp', 'emotion'))
fv4 <- summarySE(fv1, 'rating', groupvars = c('agegrp' , 'level' ))

# graph of emotion x age interaction(without legend)
ea <-  ggplot(fv3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 

# graph of emotion x age interaction(with legend)
eal <-  ggplot(fv3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = "top", legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 

# graph of level x age interactionwithout legend
ma <- ggplot(fv4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="LEVEL") + xlab("Age Group") + ylab (NULL) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

#mXa with legend
mal <- ggplot(fv4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='top', legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab (NULL) + 
coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

# graph of emotion x mag x age interaction
emal = ggplot(fv2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = 'top', legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Magnitude of Expression") + ylab (NULL)

#3x3x3 without legend
ema = ggplot(fv2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab (NULL)

ggsave(ema, filename = "val_ema.png", width = 11, height = 7, units = "in")
ggsave(ma, filename = "val_ma.png", width = 6, height = 7, units = "in")
ggsave(ea, filename = "val_ea.png",  width = 6, height = 7, units = "in")
ggsave(eal, filename = "val_eal.png",  width = 6, height = 7, units = "in", bg="transparent")
ggsave(emal, filename = "val_emal.png",  width = 11, height = 7, units = "in", bg="transparent")
ggsave(mal, filename = "val_emal.png",  width = 6, height = 7, units = "in", bg="transparent")
