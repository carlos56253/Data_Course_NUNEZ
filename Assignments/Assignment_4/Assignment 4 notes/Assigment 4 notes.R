library(tidyverse)

data("iris")

iris

iris[1:5,2]

iris[1:5, "Sepal.Width"]
#Sepal width it is 1 D. So tell which positions
iris$Sepal.Width[1:5]
#rows 1:5 and rows 10:15
iris[c(1:5,10:15),2]

vec <- 1:10

vec > 2

vec >= 2

vec >= 7
#== vec is = to ?
vec == 5

setosa_rows <- iris$Species == "setosa"

iris[setosa_rows,]

TRUE
FALSE
TRUE + FALSE  
1 + TRUE

sum(setosa_rows)

5 %in% vec
iris$Sepal.Length > iris$Petal.Length 

for(i in 1:150) {
iris$Sepal.Length[i] > iris$Petal.Length
}


iris$Species == "setosa"|iris$Species == "virginica"

set_vir <- iris$Species %in% c("setosa","virginica")
#another way, first row...down here

iris[set_vir,]

data("mtcars")
?mtcars

glimpse(mtcars)
#subset mtcars so that we only have rows that match:

#use filter() from tidyverse


#hp > 100 and disp < 150 


filter(mtcars, hp > 100 & disp < 150 )

newvector <- c(100:105, 108:120)
newvector2 <- 168L
filter(mtcars, hp %in% newvector)


?read.csv()

df <- read.csv("./Data/Fake_grade_data.csv")
names(df)
filter(df, Final_Project < 100)
