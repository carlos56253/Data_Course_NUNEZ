#load packages we'll use
library(tidyverse)
library(car)

#Load data
df <- read_csv("./Data/BioLog_Plate_Data.csv")
data("MplsStops") #for later
data("MplsDemo") #for later

##quick look 
glimpse(df)


#it's not in tidy yet. Need to fix it up

df_long <- pivot_longer(df, cols =starts_with("Hr_"),
             names_to= "Time",
             values_to = "Absorbance", 
             names_prefix = "Hr_")

nrow(df_long) /nrow(df) #we made it 3 times as long!

unique(df_long$Substrate)

#str_squish(df_long$Substrate) gets rid of white spaces
#clean up extra white space, not needed in this case but done anyway
df_long$Substrate <- str_squish(df_long$Substrate)



#change Time to numeric
df_long$Time <- as.numeric(df_long$Time)
#reassing first value on data set to SampleID without spaces
#change SampleID to column names......
names(df_long)[1] <- "SampleID"


#Visualization
df_long %>%
  ggplot(aes(x=Time, y=Absorbance, color=`SampleID`))+
    geom_smooth()+
    facet_wrap(~Substrate)+
  theme_minimal() 

glimpse(df_long)#saw that Time was factor not numeric...

#Make new column based on SampleID for "SampleType"

df_long <- df_long %>% 
  mutate(SampleType = case_when(SampleID %in% c("Clear_Creek", "Waste_Water")~ "Water", 
                                  SampleID %in% c("Soil_1", "Soil_2")~"Soil"))


#Replot using Sample Type
df_long %>%
  ggplot(aes(x=Time, y=Absorbance, color=`SampleType`))+
  geom_smooth()+
  facet_wrap(~Substrate)+
  theme_minimal() 









