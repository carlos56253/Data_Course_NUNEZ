# Cleaning the bird_data set
#broom package allows you to use tidy() and with that you give it a model
#and it will run a summary () take the output and turn it into a nice df. 
# %>% Is made by control shift m.
#any time you're trying to understand a new package run browseVignettes(). 

# packages
library(tidyverse)

# data set
df <- read_csv("./Data/Bird_Measurements.csv") %>% 
  select(!ends_with("_N"))

# tricky capitalization
names(df)[names(df) == "unsexed_mass"] <- "Unsexed_mass"

janitor::make_clean_names(names(df))
 
str(df)
names(df)




# convert family and mating systems to factor
df$Family <- factor(df$Family)
df$Mating_System <- factor(df$Mating_System)


# Split the data set into M/F/U
# for each, do the pivot_longer and then add a new column showing sex
Male <- df %>% select(c(1:4,20:22),starts_with("M_")) %>% 
  pivot_longer(starts_with("M_"),
               names_to="Measurement",values_to="Value",
               names_prefix = "M_") %>% 
  mutate(Sex="male")

Female <- df %>% select(c(1:4,20:22),starts_with("F_")) %>% 
  pivot_longer(starts_with("F_"),
               names_to="Measurement",values_to="Value",
               names_prefix = "F_") %>% 
  mutate(Sex="female")

Unsexed <- df %>% select(c(1:4,20:22),starts_with("Unsexed_"))%>% 
  pivot_longer(starts_with("Unsexed_"),
               names_to="Measurement",values_to="Value",
               names_prefix = "Unsexed_") %>% 
  mutate(Sex="Unsexed")



Female 
Male
Unsexed
# stick them back together. Since they have all the same colnames now,
# we can just stack them on top of each other
full <- rbind(Female,Male,Unsexed)

# example ugly plot
full %>% 
  filter(Family %in% c("11","12","13","4","5")) %>% 
  ggplot(aes(x=Family,y=Value)) +
  geom_boxplot() +
  facet_wrap(~Measurement,scales = "free")




# Joining two data frames together based on shared column(s)
# used when not all column names are identical
library(carData)
tibble(MplsStops)
tibble(MplsDemo)

names(MplsStops)
names(MplsDemo)

full_MPLS <- full_join(MplsStops,MplsDemo,by="neighborhood") # can be by more than 1 column
names(full_MPLS)
full_MPLS %>% select(names(full_MPLS)[c(1:15,16,18)])
