#Exercises Day 2
library(here)
library(tidyverse)
library(readxl)
d <- read_csv(here("Data","bwt_matched_cleaned.csv"))

#Exercise 1

#1) Use group_by() and summarise() to calculate the mean birth weight by race and smoking status.
#2) Use ggplot() and geom_bar() to create a basic bar plot of race vs mean birth weight at first birth.
#3) Add a title and axis labels.
#4) Change the fill of the bars by smoking status set manual fills.
#5) (Bonus) use geom_text() to add the means to each bar.
#6) (Bonus) use scale_x_discrete to clean the x-axis tick labels

summarize_data <- d %>% 
  group_by(race,smoke_1) %>% 
  summarise(mean_bwt_1 = mean(bwt_1, na.rm = TRUE)) %>%
  drop_na(race)
  
summarize_data %>%   
  ggplot(aes(x=race,y=mean_bwt_1, fill = smoke_1)) + 
  ggtitle("race vs mean birth weight at first birth -- grouped by smoking status") +
  xlab("Race") + ylab("Mean Birth Weight") +
  geom_bar(stat="identity",position = position_dodge()) + 
  geom_text(aes(label=round(mean_bwt_1,1)), vjust=1, color="white",position = position_dodge(.9), size=3)

#Exercise 2


#1) Use ggplot() and geom_boxplot() to create a basic box plot of race vs material weight at first birth.
#2) Add a title and axis labels.
#3) Change the fill of the boxes by history of hypertension and set manual fills.
#4) Use scale_x_discrete to clean the x-axis tick labels
#5) (Bonus) Show the outliers and change their ascetics
#(Hint) outlier.shape is one of the parameters you can set within geom_boxplot(), try googling the other options.

noNAs <- d %>%
  drop_na(c("race", "ht_1"))


ggplot(noNAs,aes(x=race,y=lwt_1, fill=ht_1)) +
  ggtitle("Box Plots of Race Versus maternal weight at first birth Grouped by history of hypertension") +
  xlab("Race") + 
  ylab("maternal weight at first birth") +
  geom_boxplot(outlier.shape=20, outlier.size=2, outlier.color = "red") +
  scale_fill_manual(values = c("orchid3","deepskyblue3"),
                    name="History of Hypertension") +
  scale_x_discrete(labels=c("Black", "Other", "White"))


# Exercise 3


#1) Use ggplot() and geom_density() to create a basic density plot of maternal age at second birth.
#2) Add a title and axis labels.
#3) Change the fill of the curves by History of Pre-term Labor at Second Birth and set manual fills.
#4) Change the color of the curves by History of Pre-term Labor at Second Birth and set manual colors.

ggplot(d,aes(x=age_2,fill=ptd_2,color=ptd_2)) +
  ggtitle("Distribution of maternal age at 2nd birth by history of preterm labour") +
  xlab("Age") + 
  ylab("Density") + 
  geom_density(alpha=.1)

ggplot(d,aes(x=age_2,fill=ptd_2,color=ptd_2)) +
  ggtitle("Distribution of maternal age at 2nd birth by history of preterm labour") +
  xlab("Age") + 
  ylab("Density") + 
  geom_histogram(alpha=.1)
