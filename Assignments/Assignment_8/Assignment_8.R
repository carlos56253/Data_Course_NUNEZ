#Load the libraries necessary

library(tidyverse)
library(modelr)
library(MASS)
library(fitdistrplus)
library(broom)

# 1 Import the data

df <- read_csv("../../Data/mushroom_growth.csv")


#Take a look at the data

head(df)

str(df)

# 2 Visualize relationships between response and predictor variables

 Light <- ggplot(df, aes(x=as.factor(Light), y=GrowthRate))+
  geom_boxplot()+
  facet_wrap(~Species)
 Light
 
 Nitrogen <- ggplot(df, aes(x=as.factor(Nitrogen), y=GrowthRate))+
  geom_boxplot()+
  facet_wrap(~Species)
  Nitrogen
 
 Humidity <- ggplot(df, aes(x=Humidity, y=GrowthRate))+
  geom_boxplot()+
  facet_wrap(~Species)
  Humidity

 Temperature<- ggplot(df,aes(x=as.factor(Temperature), y=GrowthRate))+
  geom_boxplot()+
  facet_wrap(~Species)
  Temperature

 Species <- ggplot(df, aes(x= Species, y=GrowthRate))+
  geom_boxplot()
 Species


# 3 Models that define the dependent variable GrowthRate
 
mod1 <- glm(data=df,
            formula = GrowthRate ~ Species * Light * Nitrogen)
summary(mod1)


mod2 <- aov(data=df,
            formula = GrowthRate ~ Species * Humidity * Temperature)
summary(mod2)


mod3 <- aov(data =df,
            formula = GrowthRate ~ Species * Light * Humidity)
summary(mod3)


mod4 <- aov(data=df,
         formula = GrowthRate ~ Species * Light * Nitrogen * Humidity * Temperature)
summary(mod4)


# 4 Calc the mean sq. error for each model

mean(mod1$residuals^2)
#6837.654

mean(mod2$residuals^2)
#5378.918

mean(mod3$residuals^2)
#3726.525

mean(mod4$residuals^2)
#2332.378
#Best result/ Smallest Number


# 5 mod 4 was selected because it yielded best result
mod4


# 6 add predictions based on new hypothetical values for the independent variables used in your model

#Add prediction using original data frame
df1_pred <- df %>% 
  add_predictions(mod4) 

#  Create Hypothetical data frame
hypothetical_df <-  data.frame(Light= rep(c(45,55,65), 60),
                     Humidity= rep(c("High","Low"), 90),
                     Nitrogen= rep(c(5,15,25,35,45), 36),
                     Temperature= rep(c(5,10,15,20,25,30),30),
                     Species= rep(c("P.cornucopiae","P.ostreotus"),90))
                     
#Add prediction using hypothetical data frame
df2_pred <- hypothetical_df %>% 
  add_predictions(mod4) 

# Create new column distinguishing between Real and Hypothetical data frames 
df1_pred$Prediction_Type <- "Real"
df2_pred$Prediction_Type <- "Hypothetical"

#Combining data frames
Overall_df <- full_join(df1_pred,df2_pred)

# 7 Visualizing combined data frame
ggplot(Overall_df, aes(x= Light, y=pred, color= Prediction_Type))+
  geom_point()+
  theme_minimal()+
  geom_point(aes(y= GrowthRate), color= "Black")
  
