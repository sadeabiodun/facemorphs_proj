# Face Morph Graphs (Arousal)
# 5.15.19 KLS & SA
# Updated 7.23.20
# Updated 6.3.21 JMS
# Updated 2.26.21 JMS

# Load libraries and functions
library(ggplot2); library(reshape2); library(plyr); library(dplyr); library(plotly); library(wesanderson)
source('scr/SummarySE2.R')

# ===============
# Arousal
# ===============

f <- read.csv('data/ave_faces_ratings.csv')
fa <- f[which(f$domain == 'arsl'),]

# Remove age and domain variable (missing data)
fa$domain <- NULL 
fa$age <- NULL

# Reorder age variable
fa$agegrp <- relevel(factor(fa$agegrp), 'Younger')

# Remove people with incomplete data
fa <- fa[complete.cases(fa),]

# Change from wide to long format
fa1 <- melt(fa, id.vars=c('subnum','agegrp'), value.name = 'rating')

# Make new variable for emotion
fa1$emotion <- as.factor(t(as.data.frame(strsplit(as.character(fa1$variable), '[.]')))[,1])
fa1$emotion <- revalue(fa1$emotion, c('a'= 'Angry', 's' = 'Sad', 'h' = 'Happy'))
fa1$emotion <- relevel(fa1$emotion, 'Sad')
fa1$emotion <- relevel(fa1$emotion, 'Happy')

# Make new variable for level
fa1$level <- as.factor(t(as.data.frame(strsplit(as.character(fa1$variable), '[.]')))[,2])
fa1$level <- relevel(fa1$level, 'med')
fa1$level <- relevel(fa1$level, 'low')
fa1$level <- revalue(fa1$level, c('low'= 'Low', 'med' = 'Med', 'full' = 'Full'))

# Delete variable column
fa1$variable <- NULL

# Create a summary table for graphing
fa2 <- summarySE2(fa1, measurevar = 'rating', groupvars=c('agegrp', 'emotion', 'level'))
fa3 <- summarySE2(fa1, 'rating', groupvars=c('agegrp', 'emotion'))
fa4 <- summarySE2(fa1, 'rating', groupvars = c('agegrp' , 'level' ))

# Graph of emotion x age interaction
emo_age_leg_arsl = ggplot(fa3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) #ylab ("Arousal Rating")

# -------------------------------

# Graph of level x age interaction
mag_age_leg_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")
mag_age_leg_arsl

# Graph of level x age interaction (without legend)
mag_age_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
# + ylab ("Arousal Rating")

# -------------------------------

# Graph of emotion x mag x age interaction 

emo_mag_age_leg_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=18), axis.text.x  = element_text(size=12)) + facet_wrap(~ agegrp) + xlab("Emotion") + ylab("Arousal Rating") + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + scale_fill_manual(values =wes_palette("GrandBudapest1")) + theme(text = element_text(size=18))
# Plot dimensions (for paper) are 800x478
emo_mag_age_leg_arsl

# Graph of emotion x mag x age interaction (no legend)
emo_mag_age_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
#+ ylab ("Arousal Rating")

# Save plot
#ggsave(emo_age_leg_arsl, filename = "plots/emo_age_legend_arsl.png", width = 11, height = 7, units = "in")
#ggsave(mag_age_leg_arsl, filename = "plots/mag_age_legend_arsl.png", width = 6, height = 7, units = "in")
#ggsave(mag_age_arsl, filename = "plots/mag_age_arsl.png",  width = 6, height = 7, units = "in")
#ggsave(emo_mag_age_arsl, filename = "plots/emo_mag_age_arsl.png",  width = 6, height = 7, units = "in", bg="transparent")
ggsave(emo_mag_age_leg_arsl, filename = "plots/emo_mag_age_legend_arsl.png",  width = 6, height = 7, units = "in", bg = "transparent")
