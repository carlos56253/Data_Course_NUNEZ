library(tidyverse)
library(nycflights13)
data("flights")
flights
nycflights13::flights



#filter is the best way to do it, parenthesis on the outside will print the value.
(nov_dec <- filter(flights,month %in% c(11,12)))
#not using filter still works, but its longer.......
cov_dec <- flights$month %in% c(11,12)
#syntax for the function: dataset(flights), inside brackets[rows,columns] 
#you leave columns blank, because you only want to specify the rows. 
(cov_dex <- flights[cov_dec,])


#De Morgan's law: !(x&y) = !(x|y)
#Example: If you wanted to find flights that weren't delayed on (arrival or departure) 
#by more than two hours, you could use either of the following 2 filters:
filter(flights, !(arr_delay > 120| dep_delay >120))
filter(flights, arr_delay <= 120, dep_delay <=120) 



#tibble constructs data frames
?tibble


df <- tibble(x=c(1,NA,3))
filter(df,x>1)

# is.na() is used to help determine is a value is missing. 

filter(df, is.na(x)|x>1)


#1 
filter(flights, arr_delay > 120) 

#2 ask how to do it....

head(flights)
str(flights)
flights$dest <- as.factor(flights$dest)

str(flights)






filter(flights,dest == "IAH" | "HOU") 
  filter(flights,dest %in% "HOU")

  #3
  filter(flights, carrier %in% as.factor())

  
  
  
  #Arrange() function, similar to filter, but instead of selecting rows, it changes their order. 
  arrange(flights, year, month, day)
#use desc() to re-order by a column in descending order.
  
  arrange(flights, desc(dep_delay))

  #use tibble to help
  df <- tibble(x=c(5,2,NA,4,3,9,11))
  arrange(df,x)
  
  arrange(df, desc(x))

  #Select() Function. Select columns by name
  select(flights, year, month, day) #select all columns btw year and day

  select(flights, year:day) #select all columns except those from year to day...same as first example
  
  select(flights, -(year:day)) #everything, but that
  
  #Helper functions inside select
  #starts_with("abc"): matches names that begin with "abc"
  #ends_width("xyz"): matches names that end with "xyz"
  #contains("ijk"): matches names that contain "ijk" 
  #matches("(.)\\1"): selects variables that match a regualr expression. (This one matches any variables that contain repeated characters.)
  #? num_range("x",1:3) 
  #use rename() to rename variables. variant of select that keeps all the variables that aren't explicictly mentioned.
  rename(flights, tail_num = tailnum)
  
  # the everything() function, helps you find everything specified within the select() and then moves them to the start of the data frame
  select(flights, time_hour, air_time, everything()) 
  
  #add new variables with Mutate
  #mutate(): besides selecting sets of existing columns, it's often useful to add new columns that are functions of existing columns. That's
  #the job of mutate()
  #easiest way to see all the columns is View()
  
  flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
  #create new columns within the data set and use existing columns to make the new columns  
  mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance/air_time *60)  
  
  #now refer to the columns you just created (gain_per_hour = gain/hours)
  mutate(flights_sml, gain= dep_delay - arr_delay, hours= air_time/60, gain_per_hour = gain/hours)
  
  #If you only want to keep the new variables use, transmute():
  transmute(flights, gain = dep_delay - arr_delay, hours = air_time/60, gain_per_hour = gain/hours)
  #Arithmetic operators: +,-,*,/,^ ....... x/sum(x) calculate portion of total. y-mean(y) difference from the mean
  # Modular arithmetic: %/% (integer division) and %% (remainder), where x == y * (x %/% y) + (x %% y)
  transmute(flights, dep_time, hour = dep_time %/% 100, minute = dep_time %% 100)
  # logs: log(), log2(), log10()
  # offsets: lead(), and lag(), allow you to refer to leading or lagging values. 
  (x <- 1:10)
  lag(x)  
  lead(x)  
  
  #Cumulative and rolling aggregates: cumsum(), cumprod(), cummin(), cummax(), cummean(), 
  cumsum(x)

  cummean(x)  

 #logical comparisons, < , <= , >, >= , != and == 
  
  #Ranking: start with min_rank(), is does what it says. Use desc(x) to give largest value, smallest rank.
  y <- c(1,2,2,NA, 3,4)
min_rank(y)  
min_rank(desc(y))
percent_rank(y)
cume_dist(y)


  #Grouped summaries with summarise()
summarise(flights, delay= mean(dep_delay, na.rm = TRUE))

#pair summarize() with group_by()
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

#shizz without pipes
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, 
                   count = n(),
                   dist=mean(distance, na.rm = TRUE),
                   delay=mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count> 20, dest !="HNL")

ggplot(data= delay, mapping = aes(x=dist, y= delay))+
  geom_point(aes(size=count), alpha = 1/3)+
  geom_smooth(se=FALSE)

#Another way to do it...... USING PIPES
delays <- flights %>% 
  group_by(dest) %>% 
  summarize(
    count = n(),
    dist= mean(distance, na.rm = TRUE),
    delay=mean(arr_delay, na.rm = TRUE))%>% 
    filter(count > 20, dest != "HNL")

ggplot(data= delay, mapping = aes(x=dist, y= delay))+
  geom_point(aes(size=count), alpha = 1/3)+
  geom_smooth(se=FALSE)


