library(tidyverse)
library(tidyquant)
library(quantmod)
library(prophet)
library(ggplot2)
library(dplyr)
library(finreportr)

CompanyInfo("MSFT")

MSFT_Annual_Rep <- AnnualReports("MSFT")

MSFT_Bal <- GetBalanceSheet("MSFT", 2010)

MSFT_Cash <- GetCashFlow("MSFT", 2010) 

MSFT_Income <- GetIncome("MSFT", 2010)


Dow_jones <- tq_index("DOW")

Dow_jones10 <- Dow_jones[1:10,]

Ticket <- c(Dow_jones10$symbol)









