library(tidyverse)
library(readxl)

#read in excel sheet with read_xslx
df <- read_xlsx("./Data/wide_data_example.xlsx")



#convert treatment 1 column to numeric
df$`Treatment 1` <- as.numeric(df$`Treatment 1`)



#Convert to long format 
df_long <- pivot_longer(df,2:3, names_to = "Treatment", values_to = "Mass_g", names_prefix = "Treatment ")
#                           ^col position.  ^New col name.        ^Tells r to remove this prefix 


#other way df_long <- pivot_longer(df,c("Treatment 1", "Treatment 2"), names_to = "Treatment", values_to = "Mass_g", names_prefix = "Treatment ")


#plot it 
ggplot(df_long, aes(y=Mass_g, x=Treatment))+
  geom_boxplot()


############################

df2 <- read.csv("./Data/Biolog_Plate_Data.csv")
#Time is just one variable
hr_cols <- c("Hr_24", "Hr_48", "Hr_144")
df2_long <- pivot_longer(df2, # name of dataframe object
             hr_cols, #list of columns spread over too many columns
             names_to = "Time", # new name for variable
             values_to = "Absorbance", #name our measurement
             names_prefix = "Hr_" #remove unwanted parts of variable name
             )
#when you go straight in and plot it, it won't work bc the numbers aren't characters
df2_long$Time <- as.numeric(df2_long$Time)

#Now it works
ggplot(df2_long, aes(x=Time, y=Absorbance, color=Substrate))+
  geom_smooth(se = FALSE)+
  facet_wrap(~Sample.ID)+
  theme_minimal()
