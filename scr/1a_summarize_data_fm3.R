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

# Separate social and monetary data
d3w_s <- select(d3w, -contains('lose'), -contains('win'))
d3w_m <- select(d3w, c('subnum', 'agegrp', 'age', 'domain'), contains('lose'), contains('win'))

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

# Create columns averaging sad and angry (negative faces) for social analyses
d3w2 <- d3w2 %>%
  mutate(as.full.arsl = a.full.arsl + s.full.arsl / 2,
         as.med.arsl = a.med.arsl + s.med.arsl / 2,
         as.low.arsl = a.low.arsl + s.low.arsl / 2,
         as.full.vln = a.full.vln + s.full.vln / 2,
         as.med.vln = a.med.vln + s.med.vln / 2,
         as.low.vln = a.low.vln + s.low.vln / 2)

# # Separate social and monetary data
d3w2_s <- select(d3w2, -contains('lose'), -contains('win'))
d3w2_m <- select(d3w2, c('subnum', 'agegrp', 'age'), contains('lose'), contains('win'))

# Comment out line below unless researcher desires to create csv with domain printed on separate rows
write.csv(d3w_s,'data/ave_faces_ratings_fm3_social.csv', row.names = FALSE)
write.csv(d3w_m,'data/ave_faces_ratings_fm3_monetary.csv', row.names = FALSE)
write.csv(d3w2_s, 'data/ave_faces_ratings_aro_val_fm3_social.csv', row.names = FALSE)
write.csv(d3w2_m,'data/ave_faces_ratings_aro_val_fm3_monetary.csv', row.names = FALSE)

# Create and save new dataframes for follow up ANOVAs
ya <- d3w2_m %>% filter(agegrp == "Younger")
write.csv(ya, 'data/x_follow_ups/ya.csv', row.names = FALSE)
ma <- d3w2_m %>% filter(agegrp == "Middle Age")
write.csv(ma, 'data/x_follow_ups/ma.csv', row.names = FALSE)
oa <- d3w2_m %>% filter(agegrp == "Older")
write.csv(oa, 'data/x_follow_ups/oa.csv', row.names = FALSE)

# Find average of angry and sad ratings (valence)
# a_sub_vln <- d2 %>% filter(emotion == 'a' & domain == "vln")
# a_mean_vln <- mean(a_sub_vln$rating)
# s_sub_vln <- d2 %>% filter(emotion == 's' & domain == "vln")
# s_mean_vln <- mean(s_sub_vln$rating)

# Find average of angry and sad ratings (arousal)
# a_sub_arsl <- d2 %>% filter(emotion == 'a' & domain == "arsl")
# a_mean_arsl <- mean(a_sub_arsl$rating)
# s_sub_arsl <- d2 %>% filter(emotion == 's' & domain == "arsl")
# s_mean_arsl <- mean(s_sub_arsl$rating)
