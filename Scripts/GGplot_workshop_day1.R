#Data Visualization in R using GGplot2
#Day 1

#Kristine Kroeker, Robert Balshaw, Brandon Kozak
#September 4, 2019


library(here)
library(tidyverse)
library(readxl)


DIABETES_CLASS <- read_xlsx(here::here("Data","diabetes_study_0138.xlsx"),sheet=1,col_names=TRUE)


#Filter out missing observation - complete case analysis
DIABETES_CLASS <- DIABETES_CLASS %>% filter(!sex=="Unknown/Invalid" & !admin_type == "Missing" & !admin_type == "Other")


## Example 1
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications, color=sex)) + geom_point() +
  ggtitle("Age Versus Number of Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + xlab("Age (Years)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
                     name = "Sex") +
  facet_grid(admin_type ~ .)


## Example 2
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  xlab("Age (Years)") + ylab("Weight (lbs.)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
                     name="Sex") +
  stat_density_2d(color="black") +
  facet_grid(. ~ sex) +
  theme_light()


## Example 3
#Taking a sample of 500
set.seed(15)
DIABETES_SAMPLE <- sample_n(DIABETES_CLASS,size=500)

#Create basic scatter plot  
#Create basic scatter plot  
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures, color=admin_type, shape=admin_type))  + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + ylab("Number of Lab Procedures") +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("magenta4","deeppink3","royalblue4"),
                     name = "Admission Type") +
  scale_shape_manual(values=c("circle","triangle","square"),
                     name = "Admission Type") +
  annotate(geom="text",
           x=c(90,92,89),
           y=c(100,95,90),
           label = c("Elective=109", "Emergency=296", "Urgent=95"))




#creating a black plot with title
ggplot(DIABETES_CLASS,aes(x=age,y=weight)) + 
  ggtitle(label = "Custom Title")
 

#Recreating Example 1 
  
  ## Recreating Example 1
#Final plot to recreate
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications, color=sex)) + 
  geom_point() +
  ggtitle("Age Versus Number of Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + xlab("Age (Years)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
                     name = "Sex") +
  facet_grid(admin_type ~ .)


#Create the basic scatter plot.
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications)) + 
  geom_point()


#Add a title and touch up the axis labels.
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications)) + 
  geom_point() +
  ggtitle("Age Versus Number of Distinct Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + 
  xlab("Age (Years)")


#Add color by sex.
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications, color=sex)) + 
  geom_point() +
  ggtitle("Age Versus Number of Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + 
  xlab("Age (Years)")


#Use scale_color_manual() to choose any color.
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications, color=sex)) + 
  geom_point() +
  ggtitle("Age Versus Number of Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + 
  xlab("Age (Years)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
                     name = "Sex")



#facet_grid() to create subplots of admin type
ggplot(DIABETES_CLASS, aes(x=age, y=num_medications, color=sex)) + 
  geom_point() +
  ggtitle("Age Versus Number of Medications Stratified by Sex and Admission Type") +
  ylab("Number of Distinct Named Medications") + 
  xlab("Age (Years)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
                     name = "Sex") +
  facet_grid(admin_type ~ .)



# Recreating Example 2

#The final plot
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  xlab("Age (Years)") + ylab("Weight (lbs.)") +
  scale_color_manual(values=c("magenta4","dodgerblue3"),
  name="Sex") +
  geom_density_2d(color="black") +
  facet_grid(. ~ sex) +
  theme_light()


#Start with the basic scatter plot and a title.
ggplot(DIABETES_CLASS,aes(x=age,y=weight)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex")


#Add in the colors.
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  scale_color_manual(values=c("magenta4","dodgerblue3"))


#Use geom_density_2d() to plot the 2d kernel density.
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  scale_color_manual(values=c("magenta4","dodgerblue3")) +
  geom_density_2d(color="black")


#Facet on sex.
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  scale_color_manual(values=c("magenta4","dodgerblue3")) +
  geom_density_2d(color="black") +
  facet_grid(. ~ sex)

#Lastly, we can add a theme.
ggplot(DIABETES_CLASS,aes(x=age, y=weight, color=sex)) +
  geom_point() +
  ggtitle(label = "Densities of Age Versus Weight Stratified by Sex") +
  scale_color_manual(values=c("magenta4","dodgerblue3")) +
  geom_density_2d(color="black") +
  facet_grid(. ~ sex) +
  theme_light()



# Recreating Example 3

#Taking a sample of 500 to demonstrate other plotting techniques
set.seed(15)
DIABETES_SAMPLE <- sample_n(DIABETES_CLASS,size=500)


#Final plot to recreate 
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures, color=admin_type, shape=admin_type))  + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + 
  ylab("Number of Lab Procedures") +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("magenta4","deeppink3","royalblue4"),
                     name = "Admission Type") +
  scale_shape_manual(values=c("circle","triangle","square"),
                     name = "Admission Type") +
  annotate(geom="text",
           x=c(90,92,89),
           y=c(100,95,90),
           label = c("Elective=109", "Emergency=296", "Urgent=95"))


#scatter plot including a title and axis labels.
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures))  + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + 
  ylab("Number of Lab Procedures") +
  geom_point() 


#Add the regression lines
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures)) + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + 
  ylab("Number of Lab Procedures") +
  geom_point() +
  geom_smooth(method = "lm")


#Add colors and shapes. We will also manually set the colours, shapes, and legend name.
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures, color=admin_type, shape=admin_type))  + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + 
  ylab("Number of Lab Procedures") +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("magenta4","deeppink3","royalblue4"),
                     name = "Admission Type") +
  scale_shape_manual(values=c("circle","triangle","square"),
                     name = "Admission Type")


#Add in the sample sizes via annotate()
ggplot(DIABETES_SAMPLE,aes(x=age, y=num_lab_procedures, color=admin_type, shape=admin_type))  + 
  ggtitle("Linear Models of Age Versus Number of Lab Procedures by Admission Type") +
  xlab("Age (Years)") + 
  ylab("Number of Lab Procedures") +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("magenta4","deeppink3","royalblue4"),
                     name = "Admission Type") +
  scale_shape_manual(values=c("circle","triangle","square"),
                     name = "Admission Type") +
  annotate(geom="text",
           x=c(90,92,89),
           y=c(100,95,90),
           label = c("Elective=109", "Emergency=296", "Urgent=95"))


