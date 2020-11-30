#Load the libraries needed. 
library(tidyverse)
library(modelr)
library(GGally)
library(MASS)
library(broom)

#Load first dataset, call it data frame 1. 
df1 <- read_csv("./FacultySalaries_1995.csv") 

#view data 
head(df1)

#View data structure
str(df1)

###############
# Task 1

#clean up data, first rename columns of interest and then pivot longer 
# To organize data by transforming all of the professor rank into a column
#call rank and their respective salaries into a column called salary. 

#rename columns of interest
df1 <- df1 %>% rename(Full = AvgFullProfSalary,
               Assoc = AvgAssocProfSalary,
               Assist = AvgAssistProfSalary) 
#clean up the data
Salary <-  pivot_longer(df1,-c(1:4,8:17),
                              names_to = "Rank", 
                              values_to = "Salary", names_prefix = "Rank")
                           
#Make a New table that excludes the Tier VIIB which is not of interest, using 
#the filter function. 
Salary1 <- filter(Salary, Tier == "I" | Tier == "IIA" | Tier == "IIB")

# Visualize the cleaned data showing the influences that Rank and Tier have on Salary. 
q <- ggplot(Salary1, aes(x = Rank, y= Salary, fill = Rank))+
  geom_boxplot()+
  facet_wrap(~Tier)+
  labs(x="Rank")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
q

#Use ggsave to save the plot as jpg on the current working directory. 
ggsave(filename = "NUNEZ_Fig_1.jpg",q)


###############
# Task 2

#Made an Anova table using the aov() which is a more specifc type of glm(),
#and then run a summary() function to help show how significant each independent 
#variable is in its interactions with salary.
Anova_Table <- aov(data = Salary, formula = 
                   Salary ~ State + Tier + Rank)
Anova_Table_Sum <- summary(Anova_Table)
Anova_Table_Sum

# Export txt file into current working directory. 
capture.output(Anova_Table_Sum, file = "./Salary_ANOVA_Summary.txt")


###############
# Task 3

#First load the data set.
df2 <- read_csv("./Juniper_Oils.csv")

#Then clean up using pivot longer to make the data more tidy. In this case combining 
#all the chemical name columns into one, and making another column for their 
#individual concentrations. 
df2_Chem <- pivot_longer(df2, cols = 11:33, names_to = "Chemical", values_to = "Concentration")


###############
# Task 4

#Visualize the cleaned up data. 
p <- ggplot(df2_Chem, aes(x = YearsSinceBurn, y = Concentration))+
  geom_smooth()+
  facet_wrap(~Chemical, scales = "free")+
  theme_minimal()
p

#Use ggsave to save the plot as jpg on the current working directory. 
ggsave(filename = "NUNEZ_Fig_2.jpg",p)

###############
# Task 5

#Made a glm() to help show which chemical concentrations were significantly affected by
#Years Since Burn.Using the tidy function to help make the data frame and the filter 
#function to  help select for the chemical concentrations with the most significant values. 
mod1 <- glm(data = df2_Chem, formula = Concentration ~ YearsSinceBurn * Chemical)
Model1 <- tidy(mod1)
summary(mod1)
#Answer
SigChem <- filter(Model1, p.value < 0.05)


