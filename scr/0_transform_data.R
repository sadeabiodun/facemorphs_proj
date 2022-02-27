# Script for Face Morph Variable Recoding and 3-way ANOVA
# Updated 7.23.20

# Install packages and load libraries
library(psych); library(plyr)

# Create transform function to recode data
transform <- function(data, x) data[,x] <- revalue(factor(data[,x]), map, warn_missing=FALSE)
key.list <- list(1:7)
faceKey <- make.keys(7,key.list)

# Load data and reverse code each variable individually 
d1a <- read.csv("data/base_data/Face_Morph1_raw.csv", header=TRUE, stringsAsFactors = FALSE)
d1b <- read.csv("data/base_data/Face_Morph2_raw.csv", header=TRUE, stringsAsFactors = FALSE)
d2a <- d1a[c(20,28:135)]
d2b <- d1b[c(20,28:189)]
map <- c('7'= '1', '6 - Moderately Negative'='2', '5 - Slightly Negative'='3','4 - Neutral'='4','3 - Slightly Positive'='5', '2 - Moderately Positive'='6', '1 - Very Positive'='7', '7'= '1', '6'='2', '5'='3','4'='4','3'='5', '2'='6', '1'='7')

for (i in 2:ncol(d2a)) d2a[i] <- as.numeric(as.character(transform(d2a,i)))
for (i in 2:ncol(d2b)) d2b[i] <- as.numeric(as.character(transform(d2b,i)))

# For d2a, you want to remove the outlier participant (above age limit)
d2a <- d2a[-(103),]
d2a <- d2a[6:nrow(d2a),] # this clips off excess, including survey preview trials

# Add 19 to age (specifically for d2b)
d2b <- d2b[4:nrow(d2b),]
d2b$age <- as.numeric(d2b$egb.18) + 19

# Add sub number
subnum1 <- seq(1:nrow(d2a))
d3a <- cbind(subnum1, d2a)

subnum2 <- seq(1:nrow(d2b))
d3b <- cbind(subnum2, d2b)
d4b <- d3b[c(1,165,3:164)]

# Write to csv
write.csv(d3a, 'data/base_data/Faces1_Numeric_Data_reverse.csv', row.names=FALSE)
write.csv(d4b, 'data/base_data/Faces2_Numeric_Data_reverse.csv', row.names=FALSE)