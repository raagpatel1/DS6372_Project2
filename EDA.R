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

# Load in the data.

library(aplore3)

RawData = glow_bonemed

# The data comes from glow500 with some variables added, and the var desc are in 
# glow 500. 

?glow500
?glow_bonemed

# Looking at the data
head(RawData)
summary(RawData)



