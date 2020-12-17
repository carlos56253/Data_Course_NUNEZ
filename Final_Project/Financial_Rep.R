#Load libraries
library(tidyverse)
library(tidyquant)
library(finreportr)
library(prophet)
library(patchwork)

##### Apple Stock Ratios 2017

#Load in Financial Statements for Apple 2017
AAPL_In <- GetIncome("AAPL", 2017)


AAPL_Bal <- GetBalanceSheet("AAPL",2017)

#Not used in this analysis
#AAPL_Cash <- GetCashFlow("AAPL", 2017)

# PE Ratio Process / Load data
AAPL <- tq_get("AAPL")

head(AAPL)
#Organize data
Dates <- AAPL$date
Price <- as.numeric(AAPL$open)

#rename columns
AAPL <- data.frame(Dates = Dates , Price = Price)
head(AAPL)
#filter out years
Years <- AAPL$Dates %>% 
  as.character() %>% 
  str_split("-") %>% 
  purrr::map_chr(1)

#Add new column with just years
AAPL$Years <- Years
#filter out the year wanted
Stock <- AAPL %>% filter(Years == 2017) 


# Earnings per share process, not taking into account years where there were stock splits
Earnings <- filter(AAPL_In, Metric == "Earnings Per Share, Basic") %>% tail(2)
#pick the row that doesn't have stock split
Earnings_PS <- Earnings[1,]

#Earnings per share 2017
Earnings_PS <- Earnings_PS$Amount

#PE Ratio 2017
PE_Ratio <- tail(Stock$Price, 1)/as.numeric(Earnings_PS)

#PB Ratio process 
Assets <- filter(AAPL_Bal, Metric == "Assets")

Liable <- filter(AAPL_Bal, Metric == "Liabilities")

Shares_out <- filter(AAPL_In, Metric == "Weighted Average Number of Shares Outstanding, Basic")

#PB Ratio 2017
PB_Ratio <-  (as.numeric((tail(Assets$Amount, 1))) - as.numeric((tail(Liable$Amount, 1)))) / as.numeric((tail(Shares_out$Amount, 1)))

#Current Ratio Process
Current_As <- filter(AAPL_Bal, Metric == "Assets, Current")

Current_Li <- filter(AAPL_Bal, Metric == "Liabilities, Current")

#Current Ratio 2017
Current_Ratio <- as.numeric((tail(Current_As$Amount, 1)))/ as.numeric((tail(Current_Li$Amount, 1)))
# create variables/ columns to complement data frame 
Symbol <- "AAPL"

Year <- 2017
#build data frame from ratios
Ratios <- data.frame(Symbol, Year, Earnings_PS, PE_Ratio, PB_Ratio, Current_Ratio)


###### Microsoft Stock Ratios 2017

#Load in Financial Statements for Microsoft 2017
MSFT_In <- GetIncome("MSFT", 2017)


MSFT_Bal <- GetBalanceSheet("MSFT",2017)


#MSFT_Cash <- GetCashFlow("MSFT", 2017)

#PE Ratio Process

#Load data
MSFT <- tq_get("MSFT")

head(MSFT)
#Organize data
Date <- MSFT$date
Price <- as.numeric(MSFT$open)

#Rename columns
MSFT <- data.frame(Date = Date , Price = Price)
head(MSFT)

#Filter out years
Years <- MSFT$Date %>% 
  as.character() %>% 
  str_split("-") %>% 
  purrr::map_chr(1)
#Add new column with just years
MSFT$Years <- Years
#filter out year wanted
Stock <- MSFT %>% filter(Years == 2017) 


#Earnings per share process
Earnings_PS <- filter(MSFT_In, Metric == "Earnings Per Share Basic")  %>% tail(1)

#Earnings per share 2017
Earnings_PS <- Earnings_PS$Amount

#PE Ratio 2017
PE_Ratio <- tail(Stock$Price, 1)/as.numeric(Earnings_PS)

#PB Ratio process
Assets <- filter(MSFT_Bal, Metric == "Assets")

Liable <- filter(MSFT_Bal, Metric == "Liabilities")

Share_out <- filter(MSFT_In, Metric == "Weighted Average Number Of Shares Outstanding Basic")

#PB Ratio 2017
PB_Ratio <-  (as.numeric((tail(Assets$Amount, 1))) - as.numeric((tail(Liable$Amount, 1)))) / as.numeric((tail(Share_out$Amount, 1)))


#Current Ratio process
Current_As <- filter(MSFT_Bal, Metric == "Assets Current")

Current_Li <- filter(MSFT_Bal, Metric == "Liabilities Current")

#Current Ratio 2017
Current_Ratio <- as.numeric((tail(Current_As$Amount, 1)))/ as.numeric((tail(Current_Li$Amount, 1)))

# create variables/ columns to complement data frame 
Symbol <- "MSFT"

Year <- 2017

#build data frame from ratios
Ratios1 <- data.frame(Symbol, Year, Earnings_PS, PE_Ratio, PB_Ratio, Current_Ratio)

#Combine both company ratios into one data frames
Company_Ratios <- rbind(Ratios,Ratios1)

