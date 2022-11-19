# Raag Patel setwd, make sure to comment on your copy!
# setwd("D:/School/GitHub/DS6372_Project2")

##################
# IF YOU EDIT THIS DOCUMENT, PLEASE MAKE SURE TO COMMENT WHAT YOUR LINES DO 
#################

# Make sure to throw necessary libraries up here.
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(GGally)

# Load in the data.

library(aplore3)

RawData <- glow_bonemed

# The data comes from glow500 with some variables added, and the var desc are in 
# glow 500. 

??glow500
??glow_bonemed

# Looking at the data
head(RawData)
summary(RawData)
sum(is.na(RawData))
colnames(RawData)
length(RawData)
nrow(RawData)
# fracture is our response

ggpairs(RawData)

# Age & fracscore - high corr
# Weight & bmi - high corr
# Armasist & fracture
# Fracture & sub_id
# Age & bmi 
# bonemed & fracture
# bonemed_fu & fracture
# bonetreat & fracture
RawData %>% ggplot(aes(x = age, y = fracscore)) + geom_point() + geom_smooth()
RawData %>% ggplot(aes(x = weight, y = bmi)) + geom_point() + geom_smooth()
RawData %>% ggplot(aes(x = armassist, y = fracture)) + geom_boxplot()
RawData %>% ggplot(aes(x = fracture, y = sub_id)) + geom_point() + geom_smooth()
RawData %>% ggplot(aes(x = age, y = bmi)) + geom_point() + geom_smooth()
RawData %>% ggplot(aes(x = bonemed, y = fracture)) + geom_boxplot()
RawData %>% ggplot(aes(x = bonemed_fu, y = fracture)) + geom_boxplot()
RawData %>% ggplot(aes(x = bonetreat, y = fracture)) + geom_boxplot()
RawData %>% ggplot(aes(x = fracture, y = fracscore)) + geom_boxplot()
