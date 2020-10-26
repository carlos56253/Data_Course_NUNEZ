library(tidyverse)
library(modelr)
library(MASS)
library(patchwork)


df <- modelr::heights
head(df)


mod1 <- lm(data=df, formula = income~height*sex)#* bc the slopes are different.

summary(mod1)
mean(residuals(mod1)^2)


#### "How to find the best model". 

#Start with full model
mod_full <- glm(data=df, 
                formula= income~height*weight*sex*marital*education)


#this is to help show how well predictions are captured in the data
p1 <- add_predictions(df, mod_full)%>%
  ggplot(aes(x=height, color=sex))+
  geom_point(aes(y=income))+
  geom_point(aes(y=pred), color = "black", size=.5)+
  ggtitle("mod_full")


#mod1 predictions
p2 <- add_predictions(df, mod1)%>%
  ggplot(aes(x=height, color=sex))+
  geom_point(aes(y=income))+
  geom_point(aes(y=pred), color = "black", size=.5)+
  ggtitle("mod1")


p2/p1




step <- stepAIC(mod_full)#tries all the models possible to find the one that had the best explanation. 

step$formula

#visualize step
mod_step <- glm(data = df, 
                formula = step$formula)




p3 <- add_predictions(df, mod_step)%>%
  ggplot(aes(x=height, color=sex))+
  geom_point(aes(y=income))+
  geom_point(aes(y=pred), color = "black", size=.5)+
  ggtitle("mod_step")

p1/p2/p3


#run anova to see if mod_full and mod_step are significantly different from each other

anova(mod_full, mod_step)

anova(mod_full,mod1)


#residuals
mean(residuals(mod_full)^2)

mean(residuals(mod_step)^2)


summary(mod_step)

#since anova told us they're pretty much the same, go with the more simpler one mod_step even though
#it has a higher residual






#New data set Grad School Admissions

df2 <- read_csv("./Data/GradSchool_Admissions.csv")
head(df2)

#change 1/0 to TRUE/FALSE
df2$admit <- as.logical(df2$admit)

#visualize
ggplot(df2,aes(x=gpa,y=admit))+
  geom_point()+
  geom_smooth(method = "lm")

#when using true/false and glm say family ="binomial"
mod3 <- glm(data= df2, 
            admit~gre +gpa *factor(rank), family = "binomial")


add_predictions(df2,mod3, type = "response")%>% #when you make predictions say type = response
  ggplot(aes(x=gpa,y=pred, color=gre))+
  geom_point()+
  facet_wrap(~rank)


summary(mod3)


#correlation btw rank and gre
#data analysis/statistical test this was it....
df2 %>% 
  ggplot(aes(x=factor(rank),y=gre))+
  geom_boxplot()


mod_rank <- aov(data = df2,
                formula = gre~factor(rank))
summary(mod_rank)




mod_rank2 <- aov(data = df2,
                formula = gpa~factor(rank))
summary(mod_rank2)
#today's main points:
#logistic regression is for T/F reponse....use family ="binomial" , type = "response"
#if we don't look at model and see how well it fits the data then there wil be problems. 
