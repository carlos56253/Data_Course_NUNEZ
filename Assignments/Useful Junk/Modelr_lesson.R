#####A complete data analysis on clean data


# load needed packages
library(tidyverse)
library(modelr)

# import data
df <- read_csv("./Data/designed_experiment.csv") # might be different path for you

# get a feel for data
glimpse(df)
plot(df$Plant_Mass)

# first plots

ggplot(df, aes(x=Plant_Mass,fill=Plant_Species)) + 
  geom_density(alpha=.5) +
  facet_wrap(~Water_Saturation)

ggplot(df, aes(x=Water_Saturation,y=Plant_Mass,color=Agricultural_Treatment)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~Plant_Species)



###########

#what we are looking at with mod 1
ggplot(df, aes(x=Water_Saturation,y=Plant_Mass)) + geom_smooth(method = "lm") +
  geom_point()

 # try some linear models

mod1 <- glm(data=df, #firt tell it where to find data, then give it a formula. Formulas in R are 
            #written with left and right side separated by a little tilda ~ meaning as a function of.
            Plant_Mass ~ Water_Saturation) # R will print the best fit line for when water saturation is explaining plant mass
plot(mod1$residuals)
summary(mod1) #Deviance residuals is how close to our trend line is the actual data... distance of dot to line, what's left over tha model couldn't explain
#Coeffiients: y int, and slope(for every 1% increase in water sat, expect 0.043447 grams of plant mass)



########


mod2 <- glm(data=df,
            Plant_Mass ~ Water_Saturation + Agricultural_Treatment + Plant_Species)
summary(mod2)

#what we are looking at with mod 2

ggplot(df, aes(x=Water_Saturation,y=Plant_Mass,color=Agricultural_Treatment)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~Plant_Species)


###############

mod3 <- glm(data=df,
            Plant_Mass ~ Water_Saturation * Agricultural_Treatment * Plant_Species)
summary(mod3)
###  The *  is saying to let the slope vary based on Agricultral treatment,  and let 
#the slope vary based on plant species. If you do + it just lets the intercepts vary.
# A : means the interaction between 




mod4 <- aov(data=df, #Analysis of variance 
            Plant_Mass ~ Water_Saturation * factor(Agricultural_Treatment))
summary(mod4)

plot(TukeyHSD(mod4))

# mean sq residuals//// the higher the number the worse the model is to fitting our data points
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2) # mod3 has lowest residuals

# going with mod3, let's make predictions using it...
df2 <- add_predictions(df,mod3)

ggplot(df2,aes(x=Water_Saturation,color=Agricultural_Treatment)) +
  geom_point(aes(y=Plant_Mass)) +
  geom_smooth(aes(y=Plant_Mass),method = "lm") +
  geom_point(aes(y=pred),color="Black",size=2) +
  facet_wrap(~Plant_Species)

 # show a few models
df3 <- gather_predictions(data = df,mod1,mod2,mod3)

ggplot(df3,aes(x=Water_Saturation,color=Agricultural_Treatment)) +
  geom_point(aes(y=Plant_Mass),alpha=.5) +
  geom_smooth(aes(y=Plant_Mass),method = "lm",alpha=.5) +
  geom_point(aes(y=pred),color="Black",size=2) +
  facet_wrap(~Plant_Species*model)

summary(mod3)
 
