### Graphing script for Facemorphs data 

# clear out variables and set working dir
rm(list=ls(all=TRUE))
setwd("~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs")

# load libraries and functions
library(ggplot2); library(tidyverse); library(forcats)
source('~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/scr/SummarySE.R')
# load in data and combine into one continuous sheet 
path <- "~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs/data"
list.files(path)
d1 <- read.csv('data/F1_arousal_reverse.csv', header=TRUE)
d2 <- read.csv('data/F2_arousal_reverse.csv', header=TRUE)
d3 <- rbind(d2, d1) #why does binding this in a reverse order generate an error? 
d3 <- d3[-c(208),] #removing the one participant that exceeded our age cutoff 

# update subject numbers 
d3$subnum <- seq(1:nrow(d3))

## extract mean values 
## Use SE function to get standard error values, there's probably a much more efficient way to do this (may update later)
d4 <- d3[4:12]
a1.summ <- summarySE(data = d4, measurevar = 'a.low', groupvars = NULL, na.rm = TRUE)
a2.summ <- summarySE(data = d4, measurevar = 'a.med', groupvars = NULL, na.rm = TRUE)
a3.summ <- summarySE(data = d4, measurevar = 'a.full', groupvars = NULL, na.rm = TRUE)
s1.summ <- summarySE(data = d4, measurevar = 's.low', groupvars = NULL, na.rm = TRUE)
s2.summ <- summarySE(data = d4, measurevar = 's.med', groupvars = NULL, na.rm = TRUE)
s3.summ <- summarySE(data = d4, measurevar = 's.full', groupvars = NULL, na.rm = TRUE)
h1.summ <- summarySE(data = d4, measurevar = 'h.low', groupvars = NULL, na.rm = TRUE)
h2.summ <- summarySE(data = d4, measurevar = 'h.med', groupvars = NULL, na.rm = TRUE)
h3.summ <- summarySE(data = d4, measurevar = 'h.full', groupvars = NULL, na.rm = TRUE)
se_val <- c(a1.summ$se, a2.summ$se, a3.summ$se, s1.summ$se, s2.summ$se, s3.summ$se, h1.summ$se, h2.summ$se, h3.summ$se )
mean_val <- colMeans(d4, na.rm = TRUE)

d4 <- rbind(d4, mean_val, se_val)
d4_stats <- as.data.frame(rbind(mean_val, se_val))
#set up mean arousal values as an independent data frame (for graphing)
stat_vals <- d4_stats[1,]


# separate into different categories (EMOTION, LEVEL, ETC)

#|||| ANGER RATINGS + PLOT|||# 
anger_vals <- stat_vals[c(4, 7, 1)]
anger_se_vals <- d4_stats[2, c(4, 7, 1)] 
anger_se_vals <- t(anger_se_vals)
anger_vals <- data.frame(emo_levels = factor(stat_vals[c(4, 7, 1)], levels = c(stat_vals[c(4, 7, 1)])))
anger_vals <- cbind(colnames(stat_vals[c(4, 7, 1)]), anger_vals, anger_se_vals)
colnames(anger_vals) <- c("expression_level", "arousal_rating", "SE")

## this block is needed to reorganize the order of the x axis variables 
anger_vals$expression_level <- factor(anger_vals$expression_level, levels = c("a.low", "a.med", "a.full"))
levels(anger_vals$expression_level)
anger_vals$arousal_rating <- as.numeric(as.character(anger_vals$arousal_rating))
anger_vals$SE <-  as.numeric(as.character(anger_vals$SE))

# plot it (must have tidyverse package installed for this to work)
anger_vals %>%
  mutate(name = fct_relevel(expression_level, "a.low", "a.med", "a.full")) %>% 
  ggplot( aes(x=expression_level, y = arousal_rating)) +
  geom_col(fill = "#DF0101") +
  geom_errorbar(aes(ymin = arousal_rating - SE, ymax = arousal_rating + SE), width = 0.1) + 
  xlab("Expression Level") + ylab("Arousal Rating") +
  ggtitle("Anger") + 
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  coord_cartesian(ylim= c(2,5)) 

#|||| SAD RATINGS |||#   
sad_vals <- stat_vals[c(6,9,3)]
sad_se_vals <- d4_stats[2, c(6, 9, 3)] 
sad_se_vals <- t(sad_se_vals)
sad_vals <- data.frame(emo_levels = factor(stat_vals[c(6, 9, 3)], levels = c(stat_vals[c(6, 9, 3)])))
sad_vals <- cbind(colnames(stat_vals[c(6, 9, 3)]), sad_vals, sad_se_vals)
colnames(sad_vals) <- c("expression_level", "arousal_rating", "SE")

## this block is needed to reorganize the order of the x axis variables 
sad_vals$expression_level <- factor(sad_vals$expression_level, levels = c("s.low", "s.med", "s.full"))
levels(sad_vals$expression_level)
sad_vals$arousal_rating <- as.numeric(as.character(sad_vals$arousal_rating))
sad_vals$SE <-  as.numeric(as.character(sad_vals$SE))

# plot it (must have tidyverse package installed for this to work)
sad_vals %>%
  mutate(name = fct_relevel(expression_level, "s.low", "s.med", "s.full")) %>% 
  ggplot( aes(x=expression_level, y = arousal_rating)) +
  geom_col(fill = "#0431B4" ) +
  geom_errorbar(aes(ymin = arousal_rating - SE, ymax = arousal_rating + SE), width = 0.1) + 
  xlab("Expression Level") + ylab("Arousal Rating") +
  ggtitle("Sadness") +
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  coord_cartesian(ylim= c(2,5))
  

#|||| HAPPY RATINGS |||# 
happy_vals <- stat_vals[c(5,8,2)]
happy_se_vals <- d4_stats[2, c(5, 8, 2)] 
happy_se_vals <- t(happy_se_vals)
happy_vals <- data.frame(emo_levels = factor(stat_vals[c(5, 8, 2)], levels = c(stat_vals[c(5, 8, 2)])))
happy_vals <- cbind(colnames(stat_vals[c(5, 8, 2)]), happy_vals, happy_se_vals)
colnames(happy_vals) <- c("expression_level", "arousal_rating", "SE")

## this block is needed to reorganize the order of the x axis variables 
happy_vals$expression_level <- factor(happy_vals$expression_level, levels = c("h.low", "h.med", "h.full"))
levels(happy_vals$expression_level)
happy_vals$arousal_rating <- as.numeric(as.character(happy_vals$arousal_rating))
happy_vals$SE <-  as.numeric(as.character(happy_vals$SE))

# plot it (must have tidyverse package installed for this to work)
happy_vals %>%
  mutate(name = fct_relevel(expression_level, "h.low", "h.med", "h.full")) %>% 
  ggplot( aes(x=expression_level, y = arousal_rating)) +
  geom_col(fill = "#04B404" ) +
  geom_errorbar(aes(ymin = arousal_rating - SE, ymax = arousal_rating + SE), width = 0.1) +
  xlab("Expression Level") + ylab("Arousal Rating") +
  ggtitle("Happiness") +
  theme_minimal() +
  theme( plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')) +
  coord_cartesian(ylim= c(2,5)) 



                                      