library(tidyverse)
library(tidyquant)
library(xts)
library(quantmod)
library(prophet)
library(ggplot2)

Dow_jones <- tq_index("DOW")

Ticket <- Dow_jones$symbol

divs <- xts()

for(sym in Ticket) {
  divs <- merge(divs, getDividends(sym, from = "2010-01-01",
                                   to = "2020-01-01", src = "yahoo", 
                                   auto.assign =  FALSE, auto.update = FALSE,
                                   verbose = FALSE, split.adjust = TRUE))
}



quant1 <-  getDividends("V", from = "2010-01-01",
                     to = "2020-01-01", src = "yahoo", 
                     auto.assign =  FALSE, auto.update = FALSE,
                     verbose = FALSE, split.adjust = TRUE)




#use tidyquant 

tidyquant1 <- tq_get("AAPL", get = "dividends", from = "2010-01-01")


var <- xts()

for(sym in Ticket){
  var <-  tq_get(sym, get = "dividends", from = "2010-01-01")
  
}




