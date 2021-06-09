# Script for Face Morph 3 Variable Recoding and 3-way ANOVA
# 6.3.21 JMS, adapted from 0_transform_data by KLS & SA

# Install packages and load libraries
library(psych); library(plyr); library(dplyr)

# Create transform function to recode data
transform <- function(data, x) data[,x] <- revalue(factor(data[,x]), map, warn_missing=FALSE)
key.list <- list(1:7)
faceKey <- make.keys(7,key.list)

# Load data and reverse code each variable individually 
d1 <- read.csv("data/base_data/Face_Morph3_raw.csv", header=TRUE, stringsAsFactors = FALSE)
d2 <- d1[c(29,37:160)]
map <- c('7 - Very Negative'='1', '6 - Moderately Negative'='2', '5 - Slightly Negative'='3', '4 - Neutral'='4','3 - Slightly Positive'='5', '2 - Moderately Positive'='6', '1 - Very Positive'='7', '7 - Not At All Aroused'='1', '6'='2', '5'='3','4'='4','3'='5', '2'='6', '1 - Very Aroused'='7')

for (i in 2:ncol(d2)) d2[i] <- as.numeric(as.character(transform(d2,i)))

d2 <- d2[3:nrow(d2),] # this clips off excess rows

# Add sub number
subnum <- seq(1:nrow(d2))
d3 <- cbind(subnum, d2)

# Rename columns
#rename all columns
d3 <- d3 %>% rename(c('age' = Q5, 'att_1' = Q580, 'att_2' = Q581, 'att_3' = Q585, 'att_4' = Q586,
                    '01_win_full_arsl' = Q21, '01_win_full_vln' = Q22, '01_win_low_arsl' = Q26, '01_win_low_vln' = Q27, '01_win_med_arsl' = Q31, '01_win_med_vln' = Q32,
                    '01_lose_full_arsl' = Q36, '01_lose_full_vln' = Q37, '01_lose_low_arsl' = Q41, '01_lose_low_vln' = Q42, '01_lose_med_arsl' = Q46, '01_lose_med_vln' = Q47,
                    '04_a_full_arsl' = Q50, '04_a_full_vln' = Q51, '04_a_low_arsl' = Q54, '04_a_low_vln' = Q55, '04_a_med_arsl' = Q58, '04_a_med_vln' = Q59,
                    '04_h_full_arsl' = Q62, '04_h_full_vln' = Q63, '04_h_low_arsl' = Q66, '04_h_low_vln' = Q67, '04_h_med_arsl' = Q70, '04_h_med_vln' = Q71,
                    '04_s_full_arsl' = Q74, '04_s_full_vln' = Q75, '04_s_low_arsl' = Q78, '04_s_low_vln' = Q79, '04_s_med_arsl' = Q82, '04_s_med_vln' = Q83,
                    '116_a_full_arsl' = Q86, '116_a_full_vln' = Q87, '116_a_low_arsl' = Q90, '116_a_low_vln' = Q91, '116_a_med_arsl' = Q94, '116_a_med_vln' = Q95,
                    '116_h_full_arsl' = Q98, '116_h_full_vln' = Q99, '116_h_low_arsl' = Q102, '116_h_low_vln' = Q103, '116_h_med_arsl' = Q106, '116_h_med_vln' = Q107,
                    '116_s_full_arsl' = Q110, '116_s_full_vln' = Q111, '116_s_low_arsl' = Q114, '116_s_low_vln' = Q115, '116_s_med_arsl' = Q118, '116_s_med_vln' = Q119,
                    '133_a_full_arsl' = Q122, '133_a_full_vln' = Q123, '133_a_low_arsl' = Q126, '133_a_low_vln' = Q127, '133_a_med_arsl' = Q130, '133_a_med_vln' = Q131,
                    '133_h_full_arsl' = Q134, '133_h_full_vln' = Q135, '133_h_low_arsl' = Q138, '133_h_low_vln' = Q139, '133_h_med_arsl' = Q142, '133_h_med_vln' = Q143,
                    '133_s_full_arsl' = Q146, '133_s_full_vln' = Q147, '133_s_low_arsl' = Q150, '133_s_low_vln' = Q151, '133_s_med_arsl' = Q154, '133_s_med_vln' = Q155,
                    '49_a_full_arsl' = Q158, '49_a_full_vln' = Q159, '49_a_low_arsl' = Q162, '49_a_low_vln' = Q163, '49_a_med_arsl' = Q166, '49_a_med_vln' = Q167,
                    '49_h_full_arsl' = Q170, '49_h_full_vln' = Q171, '49_h_low_arsl' = Q174, '49_h_low_vln' = Q175, '49_h_med_arsl' = Q178, '49_h_med_vln' = Q179,
                    '49_s_full_arsl' = Q182, '49_s_full_vln' = Q183, '49_s_low_arsl' = Q186, '49_s_low_vln' = Q187, '49_s_med_arsl' = Q190, '49_s_med_vln' = Q191,
                    '69_a_full_arsl' = Q194, '69_a_full_vln' = Q195, '69_a_low_arsl' = Q198, '69_a_low_vln' = Q199, '69_a_med_arsl' = Q202, '69_a_med_vln' = Q203,
                    '69_h_full_arsl' = Q206, '69_h_full_vln' = Q207, '69_h_low_arsl' = Q210, '69_h_low_vln' = Q211, '69_h_med_arsl' = Q214, '69_h_med_vln' = Q215,
                    '69_s_full_arsl' = Q218, '69_s_full_vln' = Q219, '69_s_low_arsl' = Q222, '69_s_low_vln' = Q223, '69_s_med_arsl' = Q226, '69_s_med_vln' = Q227,
                    '113_a_full_arsl' = Q230, '113_a_full_vln' = Q231, '113_a_low_arsl' = Q234, '113_a_low_vln' = Q235, '113_a_med_arsl' = Q238, '113_a_med_vln' = Q239,
                    '113_h_full_arsl' = Q242, '113_h_full_vln' = Q243, '113_h_low_arsl' = Q246, '113_h_low_vln' = Q247, '113_h_med_arsl' = Q250, '113_h_med_vln' = Q251,
                    '113_s_full_arsl' = Q254, '113_s_full_vln' = Q255, '113_s_low_arsl' = Q258, '113_s_low_vln' = Q259, '113_s_med_arsl' = Q262, '113_s_med_vln' = Q263))

# Write to csv
write.csv(d3, 'data/base_data/Faces3_Numeric_Data_reverse.csv', row.names=FALSE)