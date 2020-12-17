#Load Libraries 
library(tidyverse)
library(tidyquant)
library(finreportr)
library(prophet)
library(patchwork)

#####Stock price predictions for Apple

#Load in price data
AAPL_Pred <- tq_get("AAPL", from = "2010-01-01", to = "2017-09-30")
#view data
head(AAPL_Pred)

#Assign Variables to fit Prophet requirements
ds <- AAPL_Pred$date
y <- as.numeric(AAPL_Pred$open)

#Create new data frame that will work with Prophet
AAPL_Pred <- data.frame(ds=ds, y=y)
#view data
head(AAPL_Pred)

#Use Prophet to make stock price predictions for 5 years
mod1 <- prophet(AAPL_Pred)
future <- make_future_dataframe(mod1, period = 520)
Predictions <- predict(mod1, future)

#Plot predictions
plot(mod1,Predictions, main = "Apple Stock Price / Predictions", xlabel = "Date", ylabel = "Apple Stock Price")

#Save as variable
Apple_Pre <-  plot(mod1,Predictions, main = "Apple Stock Price / Predictions", xlabel = "Date", ylabel = "Apple Stock Price")



#####Stock price predictions for Microsoft

#Load in price data
MSFT_Pred <- tq_get("MSFT", from = "2010-01-01", to = "2017-09-30")
#View data
head(MSFT_Pred)

#Assign Variables to fit Prophet requirements
ds <- MSFT_Pred$date
y <- as.numeric(MSFT_Pred$open)

#Create new data frame that will work with Prophet
MSFT_Pred <- data.frame(ds=ds, y=y)
#view data
head(MSFT_Pred)

#Use Prophet to make stock price predictions for 5 years
mod2 <- prophet(MSFT_Pred)
future <- make_future_dataframe(mod2, period = 520)
Predictions1 <- predict(mod2, future)

#Plot predictions
plot(mod2, Predictions1, main="Microsoft stock price / Predictions", xlabel = "Date", ylabel = "Microsoft Stock Price")

#Assign variable
Microsoft_Pre <- plot(mod2, Predictions1, main="Microsoft stock price / Predictions", xlabel = "Date", ylabel = "Microsoft Stock Price")


#Both predictions side by side
Apple_Pre + Microsoft_Pre
