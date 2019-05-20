# Summarize table for JASP analyses
# 11.28.18 KLS & SA

# clear out variables and set working dir
rm(list=ls(all=TRUE))
setwd("~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs")

# load libraries and functions
library(reshape2); library(dplyr)
#source('~/Dropbox (Personal)/Functions/SummarySE.R')
source('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/scr/SummarySE.R')
# load data
d1 <- read.csv('data/Faces2_Numeric_Data.csv', header=TRUE)

# melt
d2 <- melt(d1, id.vars = c('subnum', 'age'), value.name = 'rating')

# pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# -------------------------------
# separate and summarize - arousal
# -------------------------------

d3 <- d2[which(d2$domain == 'arsl'),]
d4 <- summarySE(data = d3, measurevar = 'rating', groupvars = c('subnum', 'age', 'emotion', 'level'), na.rm = TRUE)

# make age groups
d4$agegrp <- ntile(d4$age, 3)
d4$agegrp <- factor(d4$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

# reorganize table and make wide
d4 <- d4[c(1,10,2:6)]
d4$emo_level <- interaction(d4$emotion, d4$level) # create interaction term
d7 <- dcast(d4, subnum + agegrp + age ~ emo_level, value.var = 'rating') #make wide

write.csv(d7,'data/F2_arousal.csv', row.names = FALSE)

# -------------------------------
# separate and summarize - valence
# -------------------------------

d5 <- d2[which(d2$domain == 'vln'),]
d6 <- summarySE(d5, measurevar = 'rating', groupvars = c('subnum', 'age', 'emotion', 'level'), na.rm = TRUE)

# age groups 
d6$agegrp <- ntile(d6$age, 3)
d6$agegrp <- factor(d6$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

d6 <- d6[c(1,10,2:6)]
d6$emo_level <- interaction(d6$emotion, d6$level)
d8 <- dcast(d6, subnum + agegrp + age ~ emo_level, value.var = 'rating') #long to wide 

write.csv(d8,'data/F2_valence.csv', row.names = FALSE)

# -------------------------------
# separate and summarize - natural appearance
# -------------------------------

d9 <- d2[which(d2$domain == 'nat'),]
d10 <- summarySE(d9, measurevar = 'rating', groupvars = c('subnum', 'age', 'emotion', 'level'), na.rm = TRUE)

# age groups 
d10$agegrp <- ntile(d10$age, 3)
d10$agegrp <- factor(d10$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

d10 <- d10[c(1,10,2:6)]
d10$emo_level <- interaction(d10$emotion, d10$level)
d11 <- dcast(d10, subnum + agegrp + age ~ emo_level, value.var = 'rating') #long to wide 

write.csv(d11,'data/F2_natural.csv', row.names = FALSE)
