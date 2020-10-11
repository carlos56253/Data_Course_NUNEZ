#load packages
library(tidyverse)
library(skimr)

#read in ML() data set
df <- read_csv("./Data/MLO_Metadata.csv")

#quick look at data
glimpse(df)
skim(df)



#dplyr verbs####
filter() #filter rows based on some criteria

select() #picks columns 
 
arrange() #arranges data frame based on column

mutate() #create or modify columns as a function of exsisting columns

group_by() #create groups based on a column

summarize() #summarize....usually based on group variables


%>% # pipe - output of left side becomes FIRST ARGUMENT of right side function
  
  df
glimpse(df)
df %>% glimpse()

#normal way
round(mean(df$Day +5),2)
#pipe way
df$Day %>% 
  mean() %>% 
  round(2)



#filter

names(df)
df2 <- df %>% 
  # trevor way filter(Month == "April" | Month =="May")
  filter(Month %in% c("April", "May")) %>% 
  select(where(is.numeric)) %>% 
  arrange(MLO_Aerosol_cm3) %>% 
  mutate(MLO_Aero_Scat_Mean = sum(MLO_aero_scat1,MLO_aero_scat2,MLO_aero_scat3)/3,
    Year_minus_5 = Year - 5,
    cm3_rounded = round(MLO_Aerosol_cm3,0),
    YMYday = paste0(Year,Mday,Yday))

    
    
    df2 %>% 
      group_by(Year) %>% 
      summarize(Mean_cm3 = mean(MLO_Aerosol_cm3),
                Min_cm3 = min(MLO_Aerosol_cm3),
                Max_cm3 = max(MLO_Aerosol_cm3),
                N_obs=n()) %>% 
      ggplot(aes(x=Year, y=Mean_cm3, size= N_obs))+
      geom_point()

    
    df %>% 
      select(starts_with("M"))
    
    df %>% 
      select(contains("aero")) %>% 
      arrange(desc(MLO_Aerosol_cm3))
    