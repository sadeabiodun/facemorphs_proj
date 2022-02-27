# Summarize Table for JASP Analyses
# 11.28.18 KLS & SA
# Updated 7.23.20 SA

# Load libraries and functions
library(reshape2); library(plyr); library(dplyr)
source('scr/SummarySE2.R')

# Load data
d1a <- read.csv('data/base_data/Faces1_Numeric_Data_reverse.csv', header=TRUE)
d1b <- read.csv('data/base_data/Faces2_Numeric_Data_reverse.csv', header=TRUE)

# Delete the 'natural' columns in Faces2 (not important for this analysis)
d1b_no_nat <- d1b[, -c(1:2)] 
d <- 1:ncol(d1b_no_nat)  
d1b_no_nat <- d1b_no_nat[, !(d%%3==0)]
d1b_2 <- cbind(d1b[c(1:2)], d1b_no_nat)
d1b_2$subnum2 <- seq(1:nrow(d1b_2)) + 100
d1b <- d1b_2

# Remember to rename subnum1 and subnum2 to subnum for subsequent analyses 
colnames(d1a)[colnames(d1a)=="subnum1"] <- "subnum"
colnames(d1b)[colnames(d1b)=="subnum2"] <- "subnum"

# Melt
d2a <- melt(d1a, id.vars = c('subnum', 'age'), value.name = 'rating')
d2b <- melt(d1b, id.vars = c('subnum', 'age'), value.name = 'rating')

# COMBINE VARIABLES! 
d2 <- rbind(d2a, d2b)

# Pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# Create [clean] csv of combined data 
write.csv(d2,'data/all_faces_ratings.csv', row.names = FALSE)

# ------------------------------- 
# Age grouping
# -------------------------------
d4a <- summarySE2(data = d2, measurevar = 'rating', groupvars = c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE)
#d4a_test <- summarySE(d2, 'rating', groupvars=c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE) 

# Make age groups
d4a$agegrp <- ntile(d4a$age, 3)
d4a$agegrp <- factor(d4a$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

# Reorganize table and make wide
d4a <- d4a[c(1,11,2:5,7,8)]
d4a$emo_level <- interaction(d4a$emotion, d4a$level) # create interaction term
d4a$emo_level_dom <- interaction(d4a$emotion, d4a$level, d4a$domain)

# Dataframe associated with combined table, domain separated in long format
d4w <- dcast(d4a, subnum + agegrp + age + domain ~ emo_level, value.var = 'rating') 

# Dataframe associated with combined wide table for valence and arousal ratings 
d4w2 <- dcast(d4a, subnum + agegrp + age ~ emo_level_dom, value.var = 'rating') 

# Output means/ranges for age groups
ya_mean <- mean(d4w2$age[which(d4w2$agegrp == 'Younger')])
ya_range <- range(d4w2$age[which(d4w2$agegrp == 'Younger')])
ma_mean <- mean(d4w2$age[which(d4w2$agegrp == 'Middle Age')])
ma_range <- range(d4w2$age[which(d4w2$agegrp == 'Middle Age')])
oa_mean <- mean(d4w2$age[which(d4w2$agegrp == 'Older')])
oa_range <- range(d4w2$age[which(d4w2$agegrp == 'Older')])
ya_length <- length(d4w2$age[which(d4w2$agegrp == 'Younger')])
ma_length <- length(d4w2$age[which(d4w2$agegrp == 'Middle Age')])
oa_length <- length(d4w2$age[which(d4w2$agegrp == 'Older')])

summary_agegrps2 <- data.frame(ya_mean, ya_range, ma_mean, ma_range, oa_mean, oa_range)

# Comment out line below unless researcher desires to create csv with domain printed on separate rows
#write.csv(d4w,'data/ave_faces_ratings.csv', row.names = FALSE)

write.csv(d4w2,'data/ave_faces_ratings_aro_val.csv', row.names = FALSE)
# Ratings from this csv output that rely on programmatic calculation of means are correct. F1 had several instances of NA's with missing subject ratings. These NA's do not impact the mean score results. 