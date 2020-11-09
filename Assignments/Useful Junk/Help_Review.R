library(tidyverse)
library(modelr)
library(MASS)#step AIC
df <- read_csv("./Data/BioLog_Plate_Data.csv")


str(df)

#Cleanit up - make longer
df <- 
df %>% pivot_longer(starts_with("Hr_"),
                    names_to = "Time",
                    values_to = "Absorbance",
                    names_prefix = "Hr_" ) %>%
  mutate(Rep = as.factor(Rep),
         Time = as.numeric(Time))
names(df)[1] <- "SampleID"

ggplot(df, aes(x=Time, y=Absorbance, color = factor(Dilution))) + 
  geom_smooth(se = FALSE)+
  facet_wrap(~Substrate)


#has more detail same results. 
mod1 <- glm(data = df, 
    formula = Absorbance ~ Substrate)

summary(mod1)


#very specific version of a glm. Use mainly glm
mod2 <- aov(data = df, 
            formula = Absorbance ~ Substrate)

summary(mod2)


#are these models different
anova(mod1,mod2) #nope




###New examples

model1 <- glm(data = df, 
            formula = Absorbance ~ Substrate * Time * factor(Dilution) * SampleID )

summary(model1)


step <- stepAIC(model1)
step$formula #how you find the best model


model2 <- glm(data = df, 
              formula = step$formula)

anova(model1, model2)
mean(residuals(model1)^2)
mean(residuals(model2)^2)


predictions <- gather_predictions(df, model1, model2)#gather to grab the data from both models. 

predictions%>%
  ggplot(aes(x=Time, color = Substrate))+
  geom_point(aes(y= Absorbance), color = "black", size =.5)+
  geom_smooth(se=FALSE, method = "lm", aes(y=pred))+###lines how model predictions, black dots will be reality......
    facet_wrap(~SampleID)

mod3 <- aov(data=df,
            formula = Absorbance ~ SampleID)
summary(mod3)

tuk <- TukeyHSD(mod3)
tuk
plot(tuk)


time1 <- Sys.time()
time2 <- Sys.time()
difftime(time1, time2)

library(tidyquant)
Ford <- tq_get("F")

names(Ford)

ggplot(Ford, aes(x=date, y=close))+
  geom_smooth()

Year <- c("2020", "2020")
Month <- c("Jan", "Feb")
Day <- c("13","17")
dates <- paste(Year, Month, Day, sep = "-")
dates <- dates %>% as.POSIXct(format = "%Y %b %d")
weekdays(dates)

?as.POSIXct


difftime(dates[1], dates[2], units = "hours")



####### Random stuff from assignment 9

```{r, echo = TRUE, message= TRUE}
a <- ggplot(df1, aes(x=rank, y=pred, color = gpa))+
  geom_point()+
  theme_minimal()+
  labs(title = "Probablity of Admission vs School Rank",subtitle = "Effect of School Rank on Admissions", x ="Rank", y= "Probablity of Admission", color = "GPA")

```

### Plot 3 & 4

```{r, echo = TRUE, message= TRUE}

b <- ggplot(df1, aes(x=rank, y=pred, color = gre))+
  geom_point()+
  theme_minimal()+
  labs(title = "Probablity of Admission vs School Rank",subtitle = "Effect of School Rank on Admissions", x ="Rank", y= "Probablity of Admission", color = "GRE")
grid.arrange(a,b, ncol = 2)

```