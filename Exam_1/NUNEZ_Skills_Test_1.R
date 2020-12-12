#Load the necessary libraries
library(tidyverse)
library(ggplot2)

#load in the data frame
df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")


#View data frame / structure
head(df)
str(df)

#1
#Histograms for Ben and Katy's DNA Concentrations
hist(df$DNA_Concentration_Katy, xlab="DNA Concentration", main = "Katy Histogram")
hist(df$DNA_Concentration_Ben, xlab= "DNA Concentration", main = "Ben Histogram")


#2
#Plot the DNA Extractions for Ben and Katy
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Katy, xlab = "Year", ylab = "DNA Concentration",main= "Katy's Extraction")
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Ben, xlab = "Year", ylab = "DNA Concentration",main= "Ben Extraction")

#3 
#save the plots as jpeg images 
jpeg("./NUNEZ_Plot1.jpg")
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Katy, xlab = "Year", ylab = "DNA Concentration",main= "Katy's Extraction")
dev.off()

jpeg("./NUNEZ_Plot2.jpg")
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Ben, xlab = "Year", ylab = "DNA Concentration",main= "Ben Extraction")
dev.off()
       

#4
# Plot comparing Ben and Katy's individual concentrations to each other
plot(df$DNA_Concentration_Ben, df$DNA_Concentration_Katy)
# Create variable that contains the ratio between Ben and Katy's DNA Concentrations
Lowcon_Ben <- df$DNA_Concentration_Ben / df$DNA_Concentration_Katy
# Create variable looking for the minimum DNA Concentration value Ben had with respect to Katy 
miniVal <- min(Lowcon_Ben)
# Create variable that displays in what position the lowest value for Ben's DNA Concentration is found
Pos <- which(miniVal == Lowcon_Ben)
#create variable containing the year Ben's DNA Concentrations were lower than Katy's 
Ben_low_year <- df$Year_Collected[Pos]

#5
#Make a data frame with only Downstairs lab
down_st <- filter(df,Lab %in% "Downstairs")
#Plot and save Ben's DNA Concentration over data collected scattered plot from the Downstairs lab
jpeg("./Ben_DNA_over_time.jpg")
plot(x = as.POSIXct(down_st$Date_Collected), y = down_st$DNA_Concentration_Ben, xlab = "Date_Collected", ylab = "DNA_Concentration_Ben")
dev.off()

# Bonus Question
#Data frame with just Year and Ben's DNA concentration average. Using the functions group by and summarize
yearly_avg <- df %>% 
  group_by(Year_Collected) %>% 
  summarize(DNA_Concentration_Ben = mean(DNA_Concentration_Ben))

# Selecting year with highest DNA concentration average
yearly_avg[which(yearly_avg$DNA_Concentration_Ben == max(yearly_avg$DNA_Concentration_Ben)),]

#Save the 12 row data frame as a csv file
write.csv(yearly_avg, "./Ben_Average_Conc.csv")


