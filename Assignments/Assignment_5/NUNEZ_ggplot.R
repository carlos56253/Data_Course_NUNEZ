library(tidyverse)
library(ggplot2)
data("iris")
#Question 1
ggplot(iris,aes(x = Sepal.Length, y= Petal.Length)) + 
  geom_point(aes(color=Species)) +
  geom_smooth(method = "lm",aes(color=Species))+
  labs(title = "Sepal length vs petal length", subtitle = "for three iris species")+
  theme_minimal()

png(file="iris_fig1.png")
ggplot(iris,aes(x = Sepal.Length, y= Petal.Length)) + 
  geom_point(aes(color=Species)) +
  geom_smooth(method = "lm",aes(color=Species))+
  labs(title = "Sepal length vs petal length", subtitle = "for three iris species")+
  theme_minimal()
dev.off()



# Question 2
ggplot(iris,aes(x=Petal.Width,fill=Species))+
  geom_density(alpha=0.5)+ 
  labs(title = "Distribution of Petal Widths", subtitle = "for three iris species", x="Petal Width")+
  theme_minimal()

png(file = "iris_fig2.png")
ggplot(iris,aes(x=Petal.Width,fill=Species))+
  geom_density(alpha=0.5)+ 
  labs(title = "Distribution of Petal Widths", subtitle = "for three iris species", x="Petal Width")+
  theme_minimal()
dev.off()


#Question 3
ggplot(iris, aes(x=Species, y=c(Petal.Width/Sepal.Width), fill=Species)) +
  geom_boxplot()+
  labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species")


png(file = "iris_fig3.png")

ggplot(iris, aes(x=Species, y=c(Petal.Width/Sepal.Width), fill=Species)) +
  geom_boxplot()+
  labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species")
dev.off()


#Question 4
iris$`newcol` <- rownames(iris) # create new column for species names
iris$dev_data <- round((iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length), 3) # compute normalized Lengths
iris <- iris[order(iris$dev_data), ] #sort
iris$`newcol` <- factor(iris$`newcol`, levels = iris$`newcol`)

ggplot(iris, aes(x=`newcol`, y=dev_data, label=dev_data)) +
  #ggplot(iris, aes(x=Sepal.Length, y=Sepal.Length -mean(Sepal.Length))) +
  geom_bar(stat='identity', aes(fill=Species), width = .5) +
  labs(subtitle="Sepal length deviance from the mean of all observations", y="Deviance from the Mean", 
       caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")+
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y=element_blank())+
  coord_flip() 
  
 png(file = "iris_fig4.png")
 ggplot(iris, aes(x=`newcol`, y=dev_data, label=dev_data)) +
   #ggplot(iris, aes(x=Sepal.Length, y=Sepal.Length -mean(Sepal.Length))) +
   geom_bar(stat='identity', aes(fill=Species), width = .5) +
   labs(subtitle="Sepal length deviance from the mean of all observations", y="Deviance from the Mean", 
        caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")+
   theme(axis.text.y = element_blank(), 
         axis.ticks = element_blank(),
         axis.title.y=element_blank())+
   coord_flip() 
 dev.off()
 
  
  
  
  
  
  
  
  
  
  

















                   