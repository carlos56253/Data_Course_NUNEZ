library(tidyverse)
library(quantmod)
library(ggplot2)

getSymbols("AAPL")
getDividends(AAPL,
             from = "1980-01-01",
             to = "2019-01-01",
             env = .GlobalEnv,
             src = "yahoo",
             auto.assign = TRUE,
             auto.update = TRUE,
             verbose = FALSE)
plot(AAPL)
