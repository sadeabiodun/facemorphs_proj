# Face Morph 3 Graphs (Arousal and Valence)
# 6.3.21 JMS, adapted from 2a_visualize_data_arsl and 2b_visualize_data_vln by KLS & SA

# Load libraries and functions
library(ggplot2); library(reshape2); library(plyr); library(dplyr); library(plotly); library(wesanderson)
source('scr/SummarySE2.R')

# ===============
# Arousal
# ===============

f <- read.csv('data/ave_faces_ratings_fm3_social.csv')
fa <- f[which(f$domain == 'arsl'),]

# Remove age, domain and attention variables (missing data)
fa$domain <- NULL 
fa$age <- NULL
fa$X1.att <- NULL
fa$X2.att <- NULL
fa$X3.att <- NULL
fa$X4.att <- NULL

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
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name = "Age Group") + xlab("Emotion") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

# -------------------------------

# Graph of level x age interaction
mag_age_leg_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name = "Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))
mag_age_leg_arsl

# Graph of level x age interaction (without legend)
mag_age_arsl = ggplot(fa4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name = "Magnitude of Expression") + xlab("Age Group") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

# -------------------------------

# Graph of emotion x mag x age interaction 

emo_mag_age_leg_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + ylab("Arousal Rating") + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + scale_fill_manual(values =wes_palette("GrandBudapest1")) + theme(text = element_text(size=20))
emo_mag_age_leg_arsl

# Graph of emotion x mag x age interaction (no legend)
emo_mag_age_arsl = ggplot(fa2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(0,7) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name = "Level of Expression") + ylab(NULL) + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1))

# Save plot
#ggsave(emo_mag_age_leg_arsl, filename = "plots/emo_mag_age_legend_arsl_fm3_social.png",  width = 11, height = 7, units = "in", bg = "transparent")

# ===============
# Valence
# ===============

f <- read.csv('data/ave_faces_ratings_fm3_social.csv')
fv <- f[which(f$domain == 'vln'),]

# Remove age, domain and attention variables (missing data)
fv$domain <- NULL 
fv$age <- NULL
fv$X1.att <- NULL
fv$X2.att <- NULL
fv$X3.att <- NULL
fv$X4.att <- NULL

# Reorder age variable
fv$agegrp <- relevel(factor(fv$agegrp), 'Younger')

# Remove people with incomplete data
fv <- fv[complete.cases(fv),]

# Change from wide to long format
fv1 <- melt(fv, id.vars = c('subnum','agegrp'), value.name = 'rating')

# Make new variable for emotion
fv1$emotion <- as.factor(t(as.data.frame(strsplit(as.character(fv1$variable), '[.]')))[,1])
fv1$emotion <- revalue(fv1$emotion, c('a'= 'Angry', 's' = 'Sad', 'h' = 'Happy'))
fv1$emotion <- relevel(fv1$emotion, 'Sad')
fv1$emotion <- relevel(fv1$emotion, 'Happy')

# Make new variable for level
fv1$level <- as.factor(t(as.data.frame(strsplit(as.character(fv1$variable), '[.]')))[,2])
fv1$level <- relevel(fv1$level, 'med')
fv1$level <- relevel(fv1$level, 'low')
fv1$level <- revalue(fv1$level, c('low'= 'Low', 'med' = 'Med', 'full' = 'Full'))

# Delete variable column
fv1$variable <- NULL

# Create a summary table for graphing
fv2 <- summarySE2(fv1, 'rating', groupvars=c('agegrp', 'emotion', 'level'))
fv3 <- summarySE2(fv1, 'rating', groupvars=c('agegrp', 'emotion'))
fv4 <- summarySE2(fv1, 'rating', groupvars = c('agegrp' , 'level' ))

# Graph of emotion x age interaction (without legend)
emo_age_val <-  ggplot(fv3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 

# Graph of emotion x age interaction (with legend)
emo_age_legend_val <-  ggplot(fv3, aes(x=emotion, y= rating, fill = agegrp)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = "top", legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Age Group") + xlab("Emotion") + ylab (NULL) 

# Graph of level x age interaction (without legend)
age_mag_val <- ggplot(fv4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="LEVEL") + xlab("Age Group") + ylab (NULL) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

# mXa (with legend)
age_mag_leg_val <- ggplot(fv4, aes (x = agegrp, y = rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +   geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + theme_minimal() + theme(legend.position='top', legend.title = element_text(size=15, face="bold"), axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + scale_fill_brewer(name  ="Magnitude of Expression") + xlab("Age Group") + ylab (NULL) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) 

# Graph of emotion x mag x age interaction
emo_mag_age_leg_val = ggplot(fv2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) + geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  ylim(1,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + ylab("Valence Rating") + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + scale_fill_manual(values =wes_palette("GrandBudapest1")) + theme(text = element_text(size=20))
emo_mag_age_leg_val

# 3x3x3 without legend
emo_mag_age_val = ggplot(fv2, aes(emotion, rating, fill = level)) + geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin = rating -se, ymax = rating + se), width=.2, position=position_dodge(.9)) + 
  coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + theme_minimal() + theme(legend.position = 'none', axis.title.x = element_text(face="bold", size=20), axis.text.x  = element_text(size=15)) + facet_wrap(~ agegrp) + xlab("Emotion") + scale_fill_brewer(name  ="Level of Expression") + ylab (NULL)

# Save plot
#ggsave(emo_mag_age_leg_val, filename = "plots/emo_mag_age_legend_val_fm3_social.png",  width = 6, height = 7, units = "in", bg = "transparent")

# ==============================
# Arousal and Valence Figure
# ==============================

# Combine dataframes
fa2$rating_type <- 'Arousal'
fv2$rating_type <- 'Valence'
all_ratings <- rbind(fa2, fv2)

all <- ggplot(all_ratings, aes(emotion, rating, fill = level)) + geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = rating - se, ymax = rating + se), width = .2, position = position_dodge(.9)) +
  ylim(1,7) + theme_minimal() + theme(legend.position = 'top', axis.title.x = element_text(face = "bold", size = 18), axis.text.x  = element_text(size = 12)) + xlab("Emotion") + ylab("Rating") + coord_cartesian(ylim=c(1, 7)) + scale_y_continuous(breaks=seq(1, 7, 1)) + scale_fill_manual(values = wes_palette("GrandBudapest1")) + theme(text = element_text(size = 18)) +
  facet_grid(rows = vars(rating_type), cols = vars(agegrp), switch = 'y')
all

# Save plot
ggsave(all, filename = "plots/fm3_social_emo_mag_age_legend.png", width = 6, height = 7, units = "in", bg = "transparent")
