library(tidyverse)

data("iris")
str(iris)

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()

df <- read.csv("./Data/Example_class.csv")


df_long <- pivot_longer(df,1:3,names_to = Days, values_to = "Grams")

df_longDays <- str_remove(df_long$Days,"Days")


df_long$Days <- as.numeric(df_long$Days)

str_replace(df_long,"Day","Your Week")


ggplot(df_long,aes(x=Days, y=Grams))+ 
  geom_col()



#Cleaning up data

library(readxl)

#Read in excel sheet with read_xlsx
df <- read_xlsx("./Data/wide_data_example.xlsx")

df

#Convert Treatment 1 column to numeric
df$`Treatment 1` <- as.numeric(df$`Treatment 1`)



df_long <- pivot_longer(df,c("Treatment 1", "Treatment 2"), 
                        names_to = "Treatment",
                        values_to = "Mass_g", 
                        names_prefix = "Treatment")


ggplot(df_long,aes(y=Mass_g,x=Treatment)) +
  geom_boxplot()

