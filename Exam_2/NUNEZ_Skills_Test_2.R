#Load the packages 
library(tidyverse)
library(ggplot2)
library(zoo)

#Import data
land_data_df <-read.csv("./landdata-states.csv")

unicef_df <-read.csv("./unicef-u5mr.csv")


#Task I 
  #Re-create the graph shown in "fig1.png"
  #Data loaded up top, take a look at data
  glimpse(land_data_df)
  #To change the y-axis values to plain numeric, add options(scipen = 999) to your script
  options(scipen = 999)
  #Visualize the data/ save as variable to export the data using ggsave()
    c <- ggplot(land_data_df, aes(x=Year, y=Land.Value, color=region))+
     theme_minimal()+
     geom_smooth()+
     scale_color_discrete("Region") +
     labs(y="Land Value (USD)")
   
  # Export it to your Exam_2 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)
   ggsave(filename = "NUNEZ_Fig_1.jpg",c)


#Task II
  #NA is a region that does not have a valid data entry, or simply doesn't contain any data. 
   #DC seems to contain all the NA Region.

  # Make a data frame that only shows NA regions. 
  NA_Regions <- land_data_df[is.na(land_data_df$region),]
 
#Task III 
     #The rest of the test uses another data set. The unicef-u5mr.csv data. Get it loaded and take a look.
     # It's not exactly tidy. You had better tidy it!

  #Loaded up top, take a look at data 
  glimpse(unicef_df)

    #Use pivot_longer function to help organize the data and make it look tidy
    df2 <- pivot_longer(unicef_df, starts_with("U5MR."), names_to = "Year", 
                        values_to = "MortalityRate", names_prefix = "U5MR.")
   
    
#Task IV
    #Re-create the graph shown in fig2.png
    #Make sure year is numeric not character
    df2$Year <- as.numeric(df2$Year)
   #Visualize data/ save it as variable to export the data using ggsave()
   d <- ggplot(df2, aes(x= Year, y= MortalityRate, color= Continent)) +
           geom_point(size=3) +
           labs(y="Mortality Rate")+
           theme_minimal()
    #Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)
   ggsave(filename = "NUNEZ_Fig_2.jpg",d)
   
   
#Task IV
   #Re-create the graph shown in fig3.png
   #Inside df2, under the MortalityRate column filter for all values that are not "NA"
   df2_NoNA <- df2 %>% 
    filter(`MortalityRate` != "NA")
    #Create a new dataframe what takes the mean Mortality Rate of all the continents
   df2_NoNA_Mean <- df2_NoNA %>%
    group_by(Continent, Year) %>%
    summarise(MortalityRate_Mean = mean(MortalityRate))
  
   #Visualize the data/ Save it as variable to export the data using ggsave()
   q <- ggplot(df2_NoNA_Mean, aes(x=Year, y=MortalityRate_Mean , color=Continent))+
     geom_line(aes(group= Continent, color= Continent),size = 2)+
     labs(y="Mean Mortality Rate (deaths per 1000 live births")+
      theme_minimal()
   #Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)
   ggsave(filename = "NUNEZ_Fig_3.jpg",q)    
   
   
#Task V
   #Re-create the graph shown in fig4.png
   df2["MortalityRate_Proportions"] <- df2$MortalityRate/1000 #calculating the proportion of Mortality rate
   
   
   
   #Visualize the data/ save as variable to export it using ggsave()  
 
   
   t <- ggplot(df2, aes(x=Year, y=MortalityRate_Proportions))+
     geom_point(aes(group=Region), color = "Blue", alpha = 0.5,size= 0.8)+ 
     facet_wrap(~Region)+
     theme_minimal()+
     theme(strip.background = element_rect(size = 0.5))+
     labs(y="Mortality Rate")
  
   #    Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)
   
    ggsave(filename = "NUNEZ_Fig_4.jpg",t, width =9, height = 9)    

#Task VI
    #Commit and push all your code and files to GitHub. I'll pull your repository at 9:30pm sharp and grade what I find.



