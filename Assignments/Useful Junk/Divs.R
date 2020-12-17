library(tidyverse)
library(tidyquant)
library(finreportr)
library(riingo)
library(prophet)
library(ggrepel)


riingo_set_token("7e2bcc36ef0f5854548349789f1de74edb5b6b64")


Dow_Jones <- tq_index("DOW")

head(Dow_Jones)
str(Dow_Jones)


test_tickers <- 
  supported_tickers() %>% 
  select(ticker) %>% 
  pull()

tickers <- Dow_Jones %>% 
  filter(symbol %in% test_tickers) %>% 
  pull(symbol)



divs_from_riingo <- 
  tickers %>% 
    riingo_prices(start_date = "1999-01-01", end_date = "2017-12-31") %>%
    arrange(ticker) %>%
    mutate(date = ymd(date))



divs_from_riingo %>% 
  select(date, ticker, close, divCash) %>% 
  head()


# The important part

divs_total <- 
  divs_from_riingo %>% 
    mutate(year = year(date)) %>% 
    group_by(year, ticker) %>% 
    mutate(div_total = sum(divCash)) %>% 
    slice(1) %>% 
    arrange(ticker) %>% 
    select(div_total)



divs_total %>% 
  group_by(ticker) %>% 
  mutate(div_increase = case_when(div_total > lag(div_total, 1) ~ 1, 
                                  TRUE ~ 0)) %>% 
  tail(10)




divs_total %>% 
  group_by(ticker) %>%
  mutate(div_increase = case_when(div_total > lag(div_total, 1) ~ 1, 
                                  TRUE ~ 0)) %>% 
  arrange(desc(year)) %>%
  arrange(ticker) %>% 
  slice(seq_len(min(which(div_increase ==0)))) %>% 
  mutate(div_inc_consec = sum(div_increase)) %>% 
  slice(1) %>% 
  arrange(desc(div_inc_consec)) %>% 
  filter(div_inc_consec > 0) %>% 
  ggplot(aes(x = reorder(ticker, div_inc_consec), y = div_inc_consec, fill = ticker)) +
  geom_col(width = .5) +
  geom_label_repel(aes(label = ticker, color = "white", nudge_y = 0.6)) +
  theme(#legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = " Dow Jones Industrial Average", y = "Years Consecutive Dividend Increase") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))








 
