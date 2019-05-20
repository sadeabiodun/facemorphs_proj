# Summarize table for JASP analyses
# 11.28.18 KLS & SA

# clear out variables and set working dir
rm(list=ls(all=TRUE))
setwd("~/Dropbox (MCAB Lab)/MCAB/Drafts/facemorphs")

# load libraries and functions
library(reshape2); library(dplyr)
#source('~/Dropbox (Personal)/Functions/SummarySE.R')
source('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/scr/SummarySE.R')
# load data
d1a <- read.csv('data/new_Faces1_Numeric_Data_reverse.csv', header=TRUE)
d1b <- read.csv('data/new_Faces2_Numeric_Data_reverse.csv', header=TRUE)

# delete the 'natural' columns in Faces2 (not important for this analysis)
d1b_no_nat <- d1b[, -c(1:2)] 
d <- 1:ncol(d1b_no_nat)  
d1b_no_nat <- d1b_no_nat[, !(d%%3==0)]
d1b_2 <- cbind(d1b[c(1:2)], d1b_no_nat)
d1b_2$subnum2 <- seq(1:nrow(d1b_2)) + 100
d1b <- d1b_2

#remember to rename subnum1 and subnum2 to subnum for subsequent analyses 
colnames(d1a)[colnames(d1a)=="subnum1"] <- "subnum"
colnames(d1b)[colnames(d1b)=="subnum2"] <- "subnum"

# melt
d2a <- melt(d1a, id.vars = c('subnum', 'age'), value.name = 'rating')
d2b <- melt(d1b, id.vars = c('subnum', 'age'), value.name = 'rating')

#COMBINE VARIABLES! 
d2 <- rbind(d2a, d2b)

# pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# create [clean] csv of combined data 
write.csv(d2,'data/all_faces_data.csv', row.names = FALSE)

# ------------------------------- 
# let's say I try to do the section below, but not separating arousal and valence into separate docs 
# -------------------------------
d4a <- summarySE(data = d2, measurevar = 'rating', groupvars = c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE)

# make age groups
d4a$agegrp <- ntile(d4a$age, 3)
d4a$agegrp <- factor(d4a$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

# reorganize table and make wide
d4a <- d4a[c(1,11,2:5,7,8)]
d4a$emo_level <- interaction(d4a$emotion, d4a$level) # create interaction term
d4w <- dcast(d4a, subnum + agegrp + age + domain ~ emo_level, value.var = 'rating') #make wide

write.csv(d4w,'data/all_faces_ratings.csv', row.names = FALSE)

