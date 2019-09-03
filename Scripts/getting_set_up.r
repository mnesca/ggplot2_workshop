#Load packages
library(tidyverse)
library(here)

#Load the data
bwt_raw <- read_csv(here("Data/bwt_matched_cleaned.csv"))

#-----------------------------CHUNK 1: Deal with improperly coded missing values----------------------------------------

bwt_clean <- bwt_raw %>% mutate(race = ifelse(race == ".",NA,race),
                                bwt_1 = ifelse(bwt_1 == 9999,NA,bwt_1),
                                bwt_2 = ifelse(bwt_2 == 9999,NA,bwt_2))
#Print
bwt_clean

#End of chunk 1

#---------------------------CHUNK 2: Obtain the mean age of mothers at first birth by race------------------------------

bwt_mean_age <- bwt_clean %>% group_by(race) %>% summarise(mean_age = mean(age_1))

#Print
bwt_mean_age

#End of chunk 2

#----------------------CHUNK 3: Create a boxplot of race vs age, set the color to race as well--------------------------

#What would happen if we didn't filter here?

boxplot <- bwt_clean %>% filter(!is.na(race)) %>% ggplot(aes(x=race,y=age_1,color=race)) + geom_boxplot()

#Print
boxplot

#End of chunk 3
