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

## Create transform function to recode data
transform <- function(data, x) data[,x] <- revalue(factor(data[,x]), map, warn_missing=FALSE)
key.list <- list(1:7)
faceKey <- make.keys(7,key.list)

## Load data and recode each variable individually 
d1 <- read.csv("data/Face_Morph1_raw.csv", header=TRUE, stringsAsFactors = FALSE)
#d1 <- read.csv("Face_Morph2_raw.csv", header=TRUE, stringsAsFactors = FALSE)
d2 <- d1[c(20,28:135)]
map <- c('7 - Very Negative'= '7', '6 - Moderately Negative'='6', '5 - Slightly Negative'='5','4 - Neutral'='4','3 - Slightly Positive'='3', '2 - Moderately Positive'='2', '1 - Very Positive'='1', '7 - Not At All Aroused'= '7', '6'='6', '5'='5','4'='4','3'='3', '2'='2', '1 - Very Aroused'='1')

for (i in 2:ncol(d2)) d2[i] <- as.numeric(as.character(transform(d2,i)))

# add sub number
subnum <- seq(1:(nrow(d1)-5))
d3 <- cbind(subnum, d2[6:nrow(d2),])

write.csv(d3, 'data/Faces1_Numeric_Data.csv', row.names=FALSE)
