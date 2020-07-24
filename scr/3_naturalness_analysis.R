##naturalness face ratings 

# load libraries and functions
library(reshape2); library(plyr); library(dplyr)
#source('~/Dropbox (Personal)/Functions/SummarySE.R')
source('scr/SummarySE2.R')

# load data

d1 <- read.csv('data/base_data/Faces2_Numeric_Data_reverse.csv', header=TRUE)

# isolate 'natural' columns in data 

d1_nat <- d1[, -c(1:2)] 
d <- 1:ncol(d1_nat)  

d1_nat <- d1_nat[, (d%%3==0)]
d1_nat2 <- cbind(d1[c(1:2)], d1_nat)
d1_nat2$subnum2 <- seq(1:nrow(d1_nat2)) + 100
d1_nat <- d1_nat2

#remember to rename subnum1 and subnum2 to subnum for subsequent analyses 
colnames(d1_nat)[colnames(d1_nat)=="subnum2"] <- "subnum"

# melt
d2 <- melt(d1_nat, id.vars = c('subnum', 'age'), value.name = 'rating')

# pull apart variable
d2$emotion <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,2])
d2$level <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,3])
d2$domain <- as.factor(t(as.data.frame(strsplit(as.character(d2$variable), '_')))[,4])

# create [clean] csv of combined data 
write.csv(d2,'data/nat_ratings.csv', row.names = FALSE)

# ------------------------------- 
# age grouping
# -------------------------------

#manual function import 

d4a <- summarySE2(data = d2, measurevar = colnames(d2[4]) , groupvars = c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE)
#d4a_test <- summarySE(d2, 'rating', groupvars=c('subnum', 'age', 'domain', 'emotion', 'level'), na.rm = TRUE) 

# make age groups
d4a$agegrp <- ntile(d4a$age, 3)
d4a$agegrp <- factor(d4a$agegrp, levels = c(1,2,3), labels = c('Younger', 'Middle Age', 'Older'))

# reorganize table and make wide
d4a <- d4a[c(1,11,2:5,7,8)]
d4a$emo_level <- interaction(d4a$emotion, d4a$level) # create interaction term
d4a$emo_level_dom <- interaction(d4a$emotion, d4a$level, d4a$domain)

# dataframe associated with combined table, domain separated in long format
d4w <- dcast(d4a, subnum + agegrp + age + domain ~ emo_level, value.var = 'rating') 

# dataframe associated with combined wide table for valence and arousal ratings 
d4w2 <- dcast(d4a, subnum + agegrp + age ~ emo_level_dom, value.var = 'rating') 

#Comment out below unless researcher desires to create csv with domain printed on separate rows
#write.csv(d4w,'data/ave_faces_ratings.csv', row.names = FALSE)

write.csv(d4w2,'data/nat_ratings.csv', row.names = FALSE)
#ratings from this csv output that rely on programmatic calculation of means are correct. F1 had several instances of NA's with missing subject ratings. These NA's do not impact the mean score results. 