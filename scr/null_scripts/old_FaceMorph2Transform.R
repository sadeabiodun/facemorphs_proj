### Script for Facemorph Variable Recoding and 3 way ANOVA 

rm(list=ls(all=TRUE))

# Install packages and load Libraries
#install.packages("psych")
#install.packages("plyr")
library(psych); library(plyr)

## Setting your working directory

### Sets the work directory to the correct location. Depending on if the data is stored locally or on a shared drive, this will have to change 
setwd("~/Dropbox (MCAB Lab)/MCAB/data/FaceMorphs")
getwd() #prints out path name (as a double check)

## Load data and recode each variable individually 
## Create transform function to recode data
transform <- function(data, x) data[,x] <- revalue(factor(data[,x]), map, warn_missing=FALSE)
key.list <- list(1:7)
faceKey <- make.keys(7,key.list)

## Load data and reverse code each variable individually 
d1 <- read.csv("data/Face_Morph2_raw.csv", header=TRUE, stringsAsFactors = FALSE)
#d1 <- read.csv("Face_Morph2_raw.csv", header=TRUE, stringsAsFactors = FALSE)
d2 <- d1[c(20,28:189)]
map <- c('7'= '1', '6 - Moderately Negative'='2', '5 - Slightly Negative'='3','4 - Neutral'='4','3 - Slightly Positive'='5', '2 - Moderately Positive'='6', '1 - Very Positive'='7', '7'= '1', '6'='2', '5'='3','4'='4','3'='5', '2'='6', '1'='7')

for (i in 2:ncol(d2)) d2[i] <- as.numeric(as.character(transform(d2,i)))
#coltype <- d1[1,28:189] (unsure why this is here)

# add 19 to age
d2 <- d2[4:nrow(d2),]
d2$age <- as.numeric(d2$egb.18) + 19

# add sub number
subnum<- seq(1:nrow(d2))
d3 <- cbind(subnum, d2)
d4 <- d3[c(1,165,3:164)]

write.csv(d4, 'data/Faces2_Numeric_Data_reverse.csv', row.names=FALSE)
