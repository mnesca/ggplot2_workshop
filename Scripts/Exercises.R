library(here)
library(tidyverse)
library(readxl)
d <- read_csv(here("Data","bwt_matched_cleaned.csv"))

# Exercise 1


#1) Create a scatter plot of the maternal age vs birth weight at first birth. 
  # Inside of ggplot() set the data and aesthetics (aes()).
#2) Set the color of the data points based on race.
  # Recall that color is an aesthetic, so you will want to add something to aes().
#3) (Bonus) Apply custom axis labels (Hint: use Google)


#Filter out missing observation - complete case analysis 
d <- d %>% 
  filter(!race=="Unknown/Invalid")

d %>%
  drop_na(race) %>%
ggplot(aes(age_1, bwt_1, color = race)) +
  geom_point() +
  xlab("age at first birth") +
  ylab("birth weight at first birth")

# Exercise 2
##Maternal Weight Versus Birth Weight at 
##First Birth Stratified by Race and Smoking Status at First Birth

#1) Create a basic scatter plot of the appropriate x and y variables
  #using ggplot() and geom_point()
#2) Color by smoking status and set manual colors using scale_color_manual()
#3) Facet by race using facet_grid().
#4) (Bonus) Inside scale_color_manual() set parameters to clean the legend.
#5) (Bonus) Using Google, figure out how to clean up the facet labels.


d %>%
  drop_na(race) %>%
ggplot(aes(lwt_1, bwt_1, color = smoke_1)) +
  geom_point() +
  facet_grid(race ~ .) +
  scale_color_manual(values=c("magenta4","dodgerblue3"), name = "Smoking Status")

#Exercise 3

# Create a 2d density plot of age versus weight stratified by sex.

#1) Start with a basic scatter plot of age vs weight.
#2) Add a title and axis labels.
#3) Use geom_density_2d() to add the 2d kernel density.
#4) Color by smoking status and set manual colors.
#5) Facet by smoking status.
#6) Add a theme
#7) (Bonus) Clean up the legend and facet labels.

d %>%
  drop_na(race) %>%
    ggplot(aes(age_1, bwt_1, color = smoke_1)) +
    ggtitle(label = "Age vs Weight") +
    geom_point() +
    facet_grid(. ~ smoke_1) +
    geom_density_2d(color="black") +
    scale_color_manual(values=c("magenta4","dodgerblue3"), name = "Smoking Status",
                       labels=c("Non Smoker", "Smoker")) + 
    theme_dark()