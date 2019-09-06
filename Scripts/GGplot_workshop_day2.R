#Data Visualization in R using GGplot2
#Day 2

#Kristine Kroeker, Robert Balshaw, Brandon Kozak
#September 6, 2019

library(here)
library(tidyverse)
library(readxl)
library(maptools)
library(lattice)
library(gridExtra)
library(ggpubr)

#import the dataset
DIABETES_CLASS <- read_xlsx(here::here("Data","diabetes_study_0138.xlsx"),sheet=1,col_names=TRUE)

#Filter to remove missing and clean the admin_source variable
DIABETES_CLASS <- DIABETES_CLASS %>% 
  filter(!sex=="Unknown/Invalid" & !admin_type == "Missing" & !admin_type == "Other")

DIABETES_CLASS <- DIABETES_CLASS %>% 
  mutate(admin_source = ifelse(admin_source == "Transfer from a skilled nursing/health care facility","Health care facility",admin_source))


## Example 1

# Using dyplr to obtian means and sd's by admin type and admin source
DIABETES_CLASS %>% 
  filter(!(admin_source=="Missing" | admin_source=="Other")) %>% 
  group_by(admin_type,admin_source) %>% 
  summarise(mean_med = mean(num_medications), sd_med = sd(num_medications)) %>%
  
#ploting 
  ggplot(aes(x=admin_type,y=mean_med,fill=admin_source)) + 
  ggtitle("Type of Admission Versus Mean Number of Medications Taken
          Grouped by Admission Source") +
  xlab("Admission Type") + ylab("Mean Number of Medications") +
  scale_fill_manual(values=c("firebrick3","seagreen4","purple3","steelblue3"), 
                    name="Admission Source") +
  geom_bar(stat="identity",position = position_dodge()) + 
  #geom_errorbar(aes(ymin=mean_med-sd_med,ymax=mean_med+sd_med),position=position_dodge(.9),width=.5) +
  geom_text(aes(label=round(mean_med,1)), vjust=1, color="white",position = position_dodge(.9), size=3)


## Example 2

# Using dyplr to obtian means and sd's by admin type and admin source
DIABETES_CLASS %>% filter( !(race=="Other" | race=="Missing") ) %>%
  
#ploting 
  ggplot(aes(x=race,y=time_in_hospital,fill=sex)) +
  geom_boxplot(outlier.color="firebrick", outlier.shape=NA, outlier.size=3.5) +
  ggtitle("Box Plots of Race Versus Time Spent in Hospital Grouped by Sex") +
  xlab("Race") + ylab("Time Spent in Hospital (Hours)") +
  scale_fill_manual(values = c("orchid3","deepskyblue3"),
                    name="Sex")

## Example 3

DIABETES_CLASS %>%
  ggplot(aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  
  geom_density(alpha=.1) +
  
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4"))


## Recreating Example 1

# Final plot to recreate
DIABETES_CLASS %>% 
  filter(!(admin_source=="Missing" | admin_source=="Other")) %>% 
  group_by(admin_type,admin_source) %>% 
  summarise(mean_med = mean(num_medications), sd_med = sd(num_medications)) %>%
  ggplot(aes(x=admin_type,y=mean_med,fill=admin_source)) + 
  ggtitle("Type of Admission Versus Mean Number of Medications Taken
          Grouped by Admission Source") +
  xlab("Admission Type") + ylab("Mean Number of Medications") +
  scale_fill_manual(values=c("firebrick3","seagreen4","purple3","steelblue3"), 
                    name="Admission Source") +
  geom_bar(stat="identity",position = position_dodge()) + 
  #geom_errorbar(aes(ymin=mean_med-sd_med,ymax=mean_med+sd_med),position=position_dodge(.9),width=.5) +
  geom_text(aes(label=round(mean_med,1)), vjust=1, color="white",position = position_dodge(.9), size=3)


## Recreating Example 1

# Using dyplr to obtian means by admin type and admin source
DIABETES_MED_MEANS <- DIABETES_CLASS %>% 
  filter(!(admin_source=="Missing" | admin_source=="Other")) %>% 
  group_by(admin_type,admin_source) %>% 
  summarise(mean_med = mean(num_medications))

#Start with the basic plot.
barplot_exp <- ggplot(DIABETES_MED_MEANS,aes(x=admin_type,y=mean_med,fill=admin_source)) + 
  ggtitle("Type of Admission Versus Mean Number of Medications Taken
          Grouped by Admission Source") +
  xlab("Admission Type") + ylab("Mean Number of Medications") +
  geom_bar(stat="identity", position = position_dodge())

#Use scale_fill_manual() to manually change the fill properties
barplot_exp <- ggplot(DIABETES_MED_MEANS,aes(x=admin_type,y=mean_med,fill=admin_source)) + 
  ggtitle("Type of Admission Versus Mean Number of Medications Taken
          Grouped by Admission Source") +
  xlab("Admission Type") + ylab("Mean Number of Medications") +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values=c("firebrick3","seagreen4","purple3","steelblue3"), 
                    name="Admission Source")

#Add the means for each bar via geom_text().
barplot_exp <- ggplot(DIABETES_MED_MEANS,aes(x=admin_type,y=mean_med,fill=admin_source)) + 
  ggtitle("Type of Admission Versus Mean Number of Medications Taken
          Grouped by Admission Source") +
  xlab("Admission Type") + ylab("Mean Number of Medications") +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values=c("firebrick3","seagreen4","purple3","steelblue3"),
                    name="Admission Source") +
  geom_text(aes(label=round(mean_med,1)),
            vjust=1, position = position_dodge(.9),size=3, color="white")



  ## Recreating Example 2
#final plot to recreate 
DIABETES_CLASS %>% filter( !(race=="Other" | race=="Missing") ) %>%
  ggplot(aes(x=race,y=time_in_hospital,fill=sex)) +
  geom_boxplot(outlier.color="firebrick", outlier.shape=NA, outlier.size=3.5) +
  ggtitle("Box Plots of Race Versus Time Spent in Hospital Grouped by Sex") +
  xlab("Race") + ylab("Time Spent in Hospital (Hours)") +
  scale_fill_manual(values = c("orchid3","deepskyblue3"),
                    name="Sex")


#First we will filter out missing and other races.
#plot the box plot
DIABETES_FILTER <- DIABETES_CLASS %>% filter( !(race=="Other" | race=="Missing") )

boxplot_exp <- ggplot(DIABETES_FILTER,aes(x=race,y=time_in_hospital,fill=sex)) +
  ggtitle("Box Plots of Race Versus Time Spent in Hospital Grouped by Sex") +
  xlab("Race") + ylab("Time Spent in Hospital (Hours)") +
  geom_boxplot(outlier.shape=NA) +
  scale_fill_manual(values = c("orchid3","deepskyblue3"),
                    name="Sex")



## Recreating Example 3
#Final plot to recreate
DIABETES_CLASS %>%
  ggplot(aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  geom_density(alpha=.1) + 
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4"))


## Recreating Example 3

#Plot the histogram via geom_histogram()
#Here we set both color and fill to map to sex
density_exp <- ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) #alpha is transparency
#Density plot is the percentage of area under the curve which in total area should be 1


#Set the aesthetics manually.
density_exp <- ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4"))



#Plot the density over each histogram via geom_density()
density_exp <- ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4")) +
  geom_density(alpha=.1)
 

## Themes
ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4")) +
  geom_density(alpha=.1) +
  #Theme modifications  
  theme(legend.background = element_rect(color="blue"),
        plot.title = element_text(family = "serif", face = "bold"),
        axis.ticks = element_line(color="red"),
        axis.ticks.length = unit(x=.25, unit="cm"),
        legend.margin = margin(t=.5,b=.5,r=1,l=1,unit="cm"))

## Themes

ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4")) +
  geom_density(alpha=.1) +
  #Theme modifications  
  theme(legend.background = element_rect(color="blue"),
        plot.title = element_text(family = "serif", face = "bold"),
        axis.ticks = element_line(color="red"),
        axis.ticks.length = unit(x=.25, unit="cm"),
        legend.margin = margin(t=.5,b=.5,r=1,l=1,unit="cm"))



#Get mean weight for each sex.
DIABETES_WEIGHT_MEANS <- DIABETES_CLASS %>% group_by(sex) %>% 
  summarise(mean_weight = mean(weight,na.rm=TRUE))

#ploting 
ggplot(DIABETES_CLASS,aes(x=weight,fill=sex,color=sex)) +
  ggtitle("                                  Distribution of Weight by Sex") +
  xlab("Weight (lbs.)") + ylab("Density") + 
  geom_histogram(mapping = aes(y=stat(density)),
                 binwidth=5,
                 position="identity",
                 alpha=.1) +
  scale_fill_manual(values = c("deeppink1","deepskyblue1")) +
  scale_color_manual(values = c("deeppink4","deepskyblue4")) +
  geom_density(alpha=.1) +
  geom_vline(data=DIABETES_WEIGHT_MEANS, 
             aes(xintercept=mean_weight,color=sex),
             linetype="dashed") +
  #Theme modifications  
  theme(legend.position = "top")


## ggarrange a dashboard of all your plots
  
ggarrange(barplot_exp,boxplot_exp, ncol = 1, nrow = 2)


