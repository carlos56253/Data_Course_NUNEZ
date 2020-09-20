df <-  read.csv("landdata-states.csv")

class(df)

head(df)

#Questions:

#  1. What other stuff does read.csv() do automatically?


# 2. How is it different from read.csv2()?
# read.csv() is used to import data that is separated by commas, while read.csv2() is for data that is separated by semicolons, and decimals are represented by commas. 

# 3. Why does read.csv2() even exist?
#Different coutnries like to use slightly different notations. 

class(df$State)

class(df$Date)

# 4. How could I change the parameters of read.csv() to make it so the class of the "State" column is "character" instead of "factor?"
 df$State <- as.character(df$State) 
class(df$State)



dim(df)


str(df)


summary(df)


# 5. What command would give the summary stats for ONLY the Home.Value column?
summary(df$Home.Value)

# 6. What value is returned by the command: names(df)[4] ?
# Just the name of the fourth column which is Home.Value
names(df)[4]



hist(df$Land.Value)  # histogram showing number of times each numeric value was seen in the vector "Land.Value"


df
# If you want to look at land value by region, you could do this:
plot(x=df$region, y=df$Land.Value)
#works with ggplot
ggplot(df, aes(y=Land.Value, x=region))+geom_boxplot()

#Find missing values
is.na(df)
#count missing data
sum(is.na(df))
#omit missing values
na.omit(df)
#remove missing values
na.rm = TRUE # tried in boxplot, but still shows NA ....


# Land value by year
plot(x=df$Year,y=df$Land.Value)
#works with ggplot....why??
ggplot(df, aes(y=Land.Value, x=Year,col=region))+geom_point()

#Could also change the shape
ggplot(df, aes(y=Land.Value, x=Year,col=region, shape=region))+geom_point()


# 7. What is happening when you add (.col=df$region) to the above plotting code?
# In other words, what happens when you run: plot(x=df$Year,y=df$Land.Value,col=df$region)
