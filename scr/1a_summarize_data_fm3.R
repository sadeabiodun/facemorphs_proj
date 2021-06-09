# Summarize Table for JASP Analyses
# 6.3.21 JMS, adapted from 1_summarize_data by KLS & SA

# Load libraries and functions
library(reshape2); library(plyr); library(dplyr)
source('scr/SummarySE2.R')

# Load data
d1 <- read.csv('data/base_data/Faces3_Numeric_Data_reverse.csv', header=TRUE)

# Remove attention check columns
d1 <- subset(d1, select = -c(att_1, att_2, att_3, att_4))

# Melt
d2 <- melt(d1, id.vars = c('subnum', 'age'), value.name = 'rating')

# Pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# Create [clean] csv of combined data
write.csv(d2,'data/all_faces_ratings_fm3.csv', row.names = FALSE)

# ------------------------------- 
# Age grouping
# -------------------------------
d3 <- summarySE2(data = d2, measurevar = 'rating', groupvars = c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE)

# Make age groups
d3$agegrp <- ntile(d3$age, 3)
d3$agegrp <- factor(d3$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

# Reorganize table and make wide
d3 <- d3[c(1,11,2:5,7,8)]
d3$emo_level <- interaction(d3$emotion, d3$level) # create interaction term
d3$emo_level_dom <- interaction(d3$emotion, d3$level, d3$domain)

# Dataframe associated with combined table, domain separated in long format
d3w <- dcast(d3, subnum + agegrp + age + domain ~ emo_level, value.var = 'rating') 

# Dataframe associated with combined wide table for valence and arousal ratings 
d3w2 <- dcast(d3, subnum + agegrp + age ~ emo_level_dom, value.var = 'rating') 

# Output means/ranges for age groups
ya_mean <- mean(d3w2$age[which(d3w2$agegrp == 'Younger')])
ya_range <- range(d3w2$age[which(d3w2$agegrp == 'Younger')])
ma_mean <- mean(d3w2$age[which(d3w2$agegrp == 'Middle Age')])
ma_range <- range(d3w2$age[which(d3w2$agegrp == 'Middle Age')])
oa_mean <- mean(d3w2$age[which(d3w2$agegrp == 'Older')])
oa_range <- range(d3w2$age[which(d3w2$agegrp == 'Older')])
ya_length <- length(d3w2$age[which(d3w2$agegrp == 'Younger')])
ma_length <- length(d3w2$age[which(d3w2$agegrp == 'Middle Age')])
oa_length <- length(d3w2$age[which(d3w2$agegrp == 'Older')])

summary_agegrps2 <- data.frame(ya_mean, ya_range, ma_mean, ma_range, oa_mean, oa_range)

# Comment out line below unless researcher desires to create csv with domain printed on separate rows
write.csv(d3w,'data/ave_faces_ratings_fm3.csv', row.names = FALSE)

write.csv(d3w2,'data/ave_faces_ratings_aro_val_fm3.csv', row.names = FALSE)