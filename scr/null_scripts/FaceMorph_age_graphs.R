# Face Morph graphs for SANS
# KLS and EAL 4/26/19
rm(list=ls(all=TRUE))

library(ggplot2); library(reshape2); library(plyr)

setwd('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/')
source('scr/SummarySE.R')
#===============
# valence 
#===============

f1v <- read.csv('data/F1_valence_reverse.csv')
f2v <- read.csv('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/data/F2_valence_reverse.csv')

# recode subnumber in f2v
f2v$subnum <- f2v$subnum + 100

# create one data frame to rule them all :D
dt <- rbind(f1v, f2v)

# remove over participant over 86
dt <- dt[-100,]

# remove age variable (missing data)
dt$age <- NULL

#get summary stats for demographics 

summary(dt)
# reorder age variable
dt$agegrp <- relevel(dt$agegrp, 'Younger')

# calculate demographic stats 
summary(dt)

## might have to do the above calculation manually for now, but will eventually have to figure out why age is a factor and not numeric 

# remove people with incomplete data
dt <- dt[complete.cases(dt),]

# change from wide to long format
dt1 <- melt(dt, id.vars=c('subnum','agegrp'), value.name = 'rating')

# make new variable for emotion
dt1$emotion <- as.factor(t(as.data.frame(strsplit(as.character(dt1$variable), '[.]')))[,1])
dt1$emotion <- revalue(dt1$emotion, c('a'= 'Angry', 's' = 'Sad', 'h' = 'Happy'))
dt1$emotion <- relevel(dt1$emotion, 'Sad')
dt1$emotion <- relevel(dt1$emotion, 'Happy')


# make new variable for level
dt1$level <- as.factor(t(as.data.frame(strsplit(as.character(dt1$variable), '[.]')))[,2])
dt1$level <- relevel(dt1$level, 'med')
dt1$level <- relevel(dt1$level, 'low')
dt1$level <- revalue(dt1$level, c('low'= 'Low', 'med' = 'Med', 'full' = 'Full'))


# delete variable column
dt1$variable <- NULL

# create a summary table for graphing
dt2 <- summarySE(dt1, 'rating', groupvars=c('agegrp', 'emotion', 'level'))
dt3 <- summarySE(dt1, 'rating', groupvars=c('agegrp', 'emotion'))
dt4 <- summarySE(dt1, 'rating', groupvars = c('agegrp' , 'level' ))

# graph of emotion x age interaction(without legend)
ea <-  ggplot(dt3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 
eal
# graph of emotion x age interaction(with legend)
eal <-  ggplot(dt3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = "top", legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 

# graph of level x age interactionwithout legend
ma <- ggplot(dt4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="LEVEL") + xlab("Age Group") + ylab (NULL) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

#mXa with legend
mal <- ggplot(dt4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='top', legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab (NULL) + 
coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

# graph of emotion x mag x age interaction
emal = ggplot(dt2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = 'top', legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Magnitude of Expression") + ylab (NULL)

#3x3x3 without legend
ema = ggplot(dt2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab (NULL)

ggsave(ema, filename = "val_ema.png", width = 11, height = 7, units = "in")
ggsave(ma, filename = "val_ma.png", width = 6, height = 7, units = "in")
ggsave(ea, filename = "val_ea.png",  width = 6, height = 7, units = "in")
ggsave(eal, filename = "val_eal.png",  width = 6, height = 7, units = "in", bg="transparent")
ggsave(emal, filename = "val_emal.png",  width = 11, height = 7, units = "in", bg="transparent")
ggsave(mal, filename = "val_emal.png",  width = 6, height = 7, units = "in", bg="transparent")
