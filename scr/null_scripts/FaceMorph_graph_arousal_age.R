### Graphing script for Facemorphs data 

# clear out variables and set working dir
rm(list=ls(all=TRUE))
setwd("~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs")

# load libraries and functions
library(ggplot2); library(tidyverse); library(forcats)

#################### BELOW IS FROM TRANSFORM SCRIPT ##############
# load libraries and functions
library(reshape2); library(dplyr)
#source('~/Dropbox (Personal)/Functions/SummarySE.R')
source('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/scr/SummarySE.R')
# load data
d1 <- read.csv('data/Faces1_Numeric_Data_reverse.csv', header=TRUE)
d25 <- read.csv('data/Faces2_Numeric_Data_reverse.csv', header=TRUE)

# have to delete the 'natural' columns in Faces2 
d26 <- d25[, -c(1:2)] 
d <- 1:ncol(d26)  
d26 <- d26[, !(d%%3==0)]
d27 <- cbind(d25[c(1:2)], d26)
d27$subnum <- seq(1:nrow(d27)) + 100

d1 <- d1[-c(98),] #removing the one participant that exceeded our age cutoff 

# quick descriptive stats for demographic (comment out if not needed)
#dstatcomb <- rbind(d1$age, d27$age)
#as.numeric(d1$age)

# melt and combine datasets (make sure subnums adjust!)
d30 <- melt(d1, id.vars = c('subnum', 'age'), value.name = 'rating')
d29 <- melt(d27, id.vars = c('subnum', 'age'), value.name = 'rating')
d2 <- rbind(d29, d30)


# pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# -------------------------------
# separate and summarize - arousal
# -------------------------------

d3 <- d2[which(d2$domain == 'arsl'),]
d4 <- summarySE(d3, measurevar = 'rating', groupvars = c('subnum', 'age', 'emotion', 'level'), na.rm = TRUE)

# make age groups
d4$agegrp <- ntile(d4$age, 3)
d4$agegrp <- factor(d4$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

###################### ABOVE IS FROM TRANSFORM SCRIPT ######################

## NOW. You're going to have to come up with some summary SE variation to extract the mean values 
## extract mean values 
## Use SE function to get standard error values, there's probably a much more efficient way to do this (may update later)
d5 <- summarySE(data = d4, measurevar = 'rating', groupvars = c('agegrp', 'emotion', 'level'), na.rm = TRUE)

# rando line to figure out subjects per age group) comment out if not needed
#subnum_younger <- summarySE(data = d4, measurevar = 'subnum', groupvars = 'agegrp')

###STOPPED HERE 

# separate into different categories (EMOTION, LEVEL, ETC)

#|||| ANGER RATINGS + PLOT|||# 
anger_vals <- d5[c(1:3,10:12,19:21),]

## this block is needed to reorganize the order of the x axis variables 
anger_vals$level <- factor(anger_vals$level, levels = c("low", "med", "full"))

# plot it (must have tidyverse package installed for this to work)
anger_vals %>%
  #mutate(name = fct_relevel(expression_level, "a.low", "a.med", "a.full")) %>% 
  ggplot( aes(x=agegrp, y = rating, fill=level)) +
  #geom_col(fill = "#DF0101") +
  geom_bar(position= "dodge", stat = "identity")+
  #geom_errorbar(aes(ymin = rating - se, ymax = rating + se), width = 0.1) + 
  xlab("Age Groups") + ylab("Arousal Rating") +
  ggtitle("Anger") + 
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  #facet_wrap(~level)
  coord_cartesian(ylim= c(2,5)) 

#|||| SAD RATINGS |||#   

sad_vals <- d5[c(7:9,16:18,25:27),]

## this block is needed to reorganize the order of the x axis variables 
sad_vals$level <- factor(sad_vals$level, levels = c("low", "med", "full"))

# plot it (must have tidyverse package installed for this to work)
sad_vals %>%
  #mutate(name = fct_relevel(expression_level, "a.low", "a.med", "a.full")) %>% 
  ggplot( aes(x=agegrp, y = rating, fill=level)) +
  #geom_col(fill = "#DF0101") +
  geom_bar(position= "dodge", stat = "identity")+
  #geom_errorbar(aes(ymin = arousal_rating - SE, ymax = arousal_rating + SE), width = 0.1) + 
  xlab("Age Groups") + ylab("Arousal Rating") +
  ggtitle("Sadness") + 
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  #facet_wrap(~level)
  coord_cartesian(ylim= c(2,5)) 
  

#|||| HAPPY RATINGS |||# 
happy_vals <- d5[c(4:6,13:15,22:24),]

## this block is needed to reorganize the order of the x axis variables 
happy_vals$level <- factor(happy_vals$level, levels = c("low", "med", "full"))

# plot it (must have tidyverse package installed for this to work)
happy_vals %>%
  #mutate(name = fct_relevel(expression_level, "a.low", "a.med", "a.full")) %>% 
  ggplot( aes(x=agegrp, y = rating, fill=level)) +
  #geom_col(fill = "#DF0101") +
  geom_bar(position= "dodge", stat = "identity")+
  #geom_errorbar(aes(ymin = arousal_rating - SE, ymax = arousal_rating + SE), width = 0.1) + 
  xlab("Age Groups") + ylab("Arousal Rating") +
  ggtitle("Happiness") + 
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  #facet_wrap(~level)
  coord_cartesian(ylim= c(2,5)) 




                                      