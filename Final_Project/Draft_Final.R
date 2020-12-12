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














trail1 <- getFinancials("MSFT", src = "tiingo", api.key = "7e2bcc36ef0f5854548349789f1de74edb5b6b64")

Dow_jones <- tq_index("DOW")

Dow_jones10 <- Dow_jones[1:10,]

Ticket <- c(Dow_jones10$symbol)

divs <- tq_get(Ticket,"dividends")

for(sym in Ticket) {

years <- divs$date %>% 
  as.character() %>% 
  str_split("-") %>% 
  purrr::map_chr(1)

divs$years <- years

yearly_divs <- divs %>% 
  group_by(years) %>% 
  summarize(Total_Yearly_Div = sum(value)) %>% 
  mutate(Stock = Ticket)

}






